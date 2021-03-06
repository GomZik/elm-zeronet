import ZeroFrame from './js/ZeroFrame'

class API extends ZeroFrame {
  constructor() {
    super()
    this.eventTarget = new EventTarget()
  }

  onOpenWebsocket() {
    this.eventTarget.dispatchEvent(new CustomEvent('zf-ready'))
  }

  onRequest(cmd, message) {
    console.log(cmd, message)
    this.eventTarget.dispatchEvent(new CustomEvent(cmd, {detail: message}))
  }

  subscribe(msg, cb) {
    this.eventTarget.addEventListener(msg, cb)
  }
}


export default {
  setup: app => {
    const api = new API()

    api.subscribe('zf-ready', () => {
      app.ports.zfReady.send(null)
    })

    app.ports.zfSend.subscribe(data => {
      console.log("zfSend:", data)

      const handler = resp => {
        console.log(resp)
        if ( data && data.reqId ) {
          app.ports.zfResponse.send({
            id: data.reqId,
            response: resp,
          })
        }
      }

      api.cmdp(data.command, data.args).then(handler, handler)
      switch (data.command) {
        case 'wrapperPushState':
          // Send to elm urlchange event
          app.ports.urlChanged.send(data.args[2])
          break
      }
    })

    api.subscribe('setSiteInfo', ev => {
      let payload = ev.detail
      console.log('got info', payload)
      app.ports.siteInfoChanged.send(payload.params)
      if (payload && payload.params && payload.params.event && payload.params.event.length > 0)
        switch (payload.params.event[0]) {
          case 'cert_changed':
            app.ports.certChanged.send(payload.params)
          case 'file_done':
            app.ports.onFileWrite.send(null)
        }
    })

    api.subscribe('wrapperPopState', ev => {
      let u = new URL(ev.detail.params.href)
      app.ports.urlChanged.send(u.search.substr(1))
    })
  }
}

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
    this.eventTarget.dispatchEvent(new CustomEvent(cmd, {detail: message}))
  }

  subscribe(msg, cb) {
    this.eventTarget.addEventListener(msg, cb)
  }

  getSiteInfo() {
    this.cmd("siteInfo", {}, info => {
      console.log('siteInfoResponse')
      this.onRequest('setSiteInfo', {params: info})
    })
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
      api.cmdp(data.command, data.args).then(resp => {
        console.log(resp)
        if ( data && data.reqId ) {
          app.ports.zfResponse.send({
            id: data.reqId,
            response: resp,
          })
        }
      })
      switch (data.command) {
        case 'wrapperPushState':
          // Send to elm urlchange event
          app.ports.urlChanged.send(data.args[2])
          break
      }
    })

    api.subscribe('setSiteInfo', ev => {
      console.log('got info', ev.detail)
      app.ports.siteInfoChanged.send(ev.detail.params)
    })

    api.getSiteInfo()
  }
}

import ZeroFrame from './js/ZeroFrame'

const api = new ZeroFrame()

export default {
  setup: app => {
    app.ports.zfSend.subscribe(data => {
      console.log("zfSend:", data)
      api.cmdp(data.command, data.args).then(resp => {
        console.log(resp)
      })
      switch (data.command) {
        case 'wrapperPushState':
          // Send to elm urlchange event
          app.ports.urlChanged.send(data.args[2])
          break
      }
    })
  }
}

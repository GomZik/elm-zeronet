import ZeroFrame from './js/ZeroFrame'

const api = new ZeroFrame()

export default {
  setup: app => {
    app.ports.zfSend.subscribe(data => {
      console.log("zfSend:", data)
      api.cmd(data.command, data.args, resp => {
        console.log(resp)
      })
    })
  }
}

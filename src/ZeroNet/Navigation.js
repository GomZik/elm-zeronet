export default {
  setup: app => {
    app.ports.goUrl.subscribe(url => {
      window.top.location.href = url
    })
  }
}

import { Elm } from './Main.elm'

import ZeroNet from './ZeroNet'

const app = Elm.Main.init({flags: { appFlags: null }})

ZeroNet.setup(app)

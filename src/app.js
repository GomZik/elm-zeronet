import { Elm } from './Main.elm'

import ZeroNet from './ZeroNet'
import Navigation from './ZeroNet/Navigation'

const app = Elm.Main.init({flags: { appFlags: null }})

ZeroNet.setup(app)
Navigation.setup(app)

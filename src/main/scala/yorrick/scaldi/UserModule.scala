package yorrick.scaldi

import scaldi.Module


class UserModule extends Module {
  bind [MessageService] to new OfficialMessageService
//  binding identifiedBy "greeting.official" to "Welcome"
}
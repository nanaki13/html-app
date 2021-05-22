package bon.jo.util

import scalajs.js
import scalajs.js.JSON

object JsonTr:
  def string(jsV: js.Any): String =
    JSON.stringify(jsV,null)
    

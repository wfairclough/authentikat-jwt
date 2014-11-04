package authentikat.jwt

import authentikat.json.SimpleJsonSerializer

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.joda.time.DateTime

case class JwtHeader(algorithm: Option[String],
                     contentType: Option[String],
                     exp: Option[DateTime],
                     typ: Option[String]) {

  def asJsonString: String = {
    val toSerialize =
      algorithm.map(x => ("alg", x)).toSeq ++
        contentType.map(x => ("cty", x)).toSeq ++
        exp.map(x => ("exp", x.getMillis)).toSeq ++
        typ.map(x => ("typ", x)).toSeq
        

    SimpleJsonSerializer(toSerialize)
  }
}

object JwtHeader {

  import org.json4s.DefaultFormats

  implicit val formats = DefaultFormats

  def apply(algorithm: String = null, contentType: String = null, expiration: DateTime = null, typ: String = "JWT"): JwtHeader = {
    JwtHeader(Option(algorithm), Option(contentType), Option(expiration), Option(typ))
  }

  def fromJsonString(jsonString: String): JwtHeader = {
    val ast = parse(jsonString)

    val alg = (ast \ "alg").extract[Option[String]]
    val cty = (ast \ "cty").extract[Option[String]]
    val exp = (ast \ "exp").extract[Option[Long]]
    val typ = (ast \ "typ").extract[Option[String]]

    JwtHeader(alg, cty, exp.map(new DateTime(_)).orElse(None), typ)
  }
}
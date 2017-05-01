
import scala.annotation.tailrec
import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._

import v1._
import v1.HotelDB._

import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      val response = route(FakeRequest(GET, "/boum")).get
      status(response) must equalTo(NOT_FOUND)
    }

    "render the index page" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/html")
      contentAsString(home) must contain ("Hotel REST API")
    }

    "return the hotels in Bangkok" in new WithApplication {
      val response = route(FakeRequest(GET, "/v1/hotels/Bangkok?apiKey=123")).get
      status(response) must equalTo(OK)
      contentType(response) must beSome.which(_ == "application/json")
      val body = contentAsString(response)
      Json.parse(body).validate[Seq[Hotel]] match {
        case s: JsSuccess[Seq[Hotel]] => s.get.forall(h => h.city must equalTo("Bangkok"))
        case e: JsError => Seq[Hotel]()
      }
    }

    "return the hotels in Bangkok in ascending order" in new WithApplication {
      val response = route(FakeRequest(GET, "/v1/hotels/Bangkok?apiKey=123&sorting=asc")).get
      status(response) must equalTo(OK)
      contentType(response) must beSome.which(_ == "application/json")
      val body = contentAsString(response)
      Json.parse(body).validate[Seq[Hotel]] match {
        case s: JsSuccess[Seq[Hotel]] => isSorted(s.get, true) must equalTo(true)
        case e: JsError => Seq[Hotel]()
      }
    }

    "return the hotels in Bangkok in descending order" in new WithApplication {
      val response = route(FakeRequest(GET, "/v1/hotels/Bangkok?apiKey=123&sorting=desc")).get
      status(response) must equalTo(OK)
      contentType(response) must beSome.which(_ == "application/json")
      val body = contentAsString(response)
      Json.parse(body).validate[Seq[Hotel]] match {
        case s: JsSuccess[Seq[Hotel]] => isSorted(s.get, false) must equalTo(true)
        case e: JsError => Seq[Hotel]()
      }
    }

    "return hotel not found in Kuala Lumpur" in new WithApplication {
      val response = route(FakeRequest(GET, "/v1/hotels/KualaLumpur?apiKey=123")).get
      status(response) must equalTo(NOT_FOUND)
      contentAsString(response) must equalTo ("No hotel found for the city")
    }

    "return bad request for request without apiKey" in new WithApplication {
      val response = route(FakeRequest(GET, "/v1/hotels/KualaLumpur")).get
      status(response) must equalTo(BAD_REQUEST)
      contentAsString(response) must equalTo ("No API key specified")
    }

    "return bad request for exceeds apiKey rateLimit" in new WithApplication {
      val responses = for(i <- 1 to 11) yield route(FakeRequest(GET, "/v1/hotels/Bangkok?apiKey=333")).get
      val response = responses.last
      status(response) must equalTo(BAD_REQUEST)
      contentAsString(response) must equalTo ("API key exceed rate limit try again later")
    }
  }

  def isSorted(hotels: Seq[Hotel], asceding: Boolean) = {
    @tailrec
    def loop(hotels: Seq[Hotel], lastPrice: Long): Boolean = {
      if (hotels.isEmpty) true
      else if (asceding) {
        if (hotels.head.price > lastPrice) loop(hotels.tail, hotels.head.price)
        else false
      } else {
        if (hotels.head.price < lastPrice) loop(hotels.tail, hotels.head.price)
        else false
      }
    }
    loop(hotels.tail, hotels.head.price)
  }
}

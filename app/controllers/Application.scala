package controllers

import play.api._
import play.api.mvc._

import v1._

class Application extends Controller {
  val db = HotelDB.instance
  val rateLimit = ApiRateLimiter.instance

  def index = Action {
    Ok(views.html.index())
  }

  def hotels(city: String, apiKey: Option[String], sorting: Option[String]) = Action {
    apiKey match {
      case Some(key) if (key.trim != "") => rateLimit.withinLimit(key) match {
        case Some(key) => db.getHotels(city, sorting) match {
          case Some(hotels) => Ok(HotelDB.json(hotels))
          case None => NotFound("No hotel found for the city")
        }
        case None => BadRequest("API key exceed rate limit try again later")
      }
      case Some(key) if (key.trim == "") => BadRequest("No API key specified")
      case None => BadRequest("No API key specified")
    }
  }
}


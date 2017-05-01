package v1

import scala.io.Source
import play.api.libs.json._
import play.api.libs.functional.syntax._

object HotelDB {
  lazy val instance = new HotelDB()

  implicit val hotelWrites = new Writes[Hotel] {
    def writes(hotel: Hotel) = Json.obj(
      "id" -> hotel.id,
      "city" -> hotel.city,
      "room" -> hotel.room,
      "price" -> hotel.price
    )
  }

  implicit val hotelReads: Reads[Hotel] = (
    (JsPath \ "id").read[Long] and
    (JsPath \ "city").read[String] and
    (JsPath \ "room").read[String] and
    (JsPath \ "price").read[Long]
  )(Hotel.apply _)

  def json(hotels: Seq[Hotel]) = {
    Json.toJson(hotels)
  }
}

case class Hotel(id: Long, city: String, room: String, price: Long)

class HotelDB() {
  val hotels = {
    val in = this.getClass.getResourceAsStream("/hoteldb.csv")
    Source.fromInputStream(in).getLines().drop(1)
      .map { row =>
        val cols = row.split(",")
        Hotel(cols(1).toLong, cols(0), cols(2), cols(3).toLong)
      }.toSeq.groupBy(_.city)
  }

  def getHotels(city: String, sorting: Option[String]) = {
    sort(hotels.get(city), sorting)
  }

  def sort(hotels: Option[Seq[Hotel]], sorting: Option[String]) = hotels match {
    case Some(hs) => sorting match {
      case Some("asc") => Some(hs.sortWith(_.price < _.price))
      case Some("desc") => Some(hs.sortWith(_.price > _.price))
      case None => hotels
    }
    case None => hotels
  }
}
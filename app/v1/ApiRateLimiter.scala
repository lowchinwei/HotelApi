package v1

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object ApiRateLimiter {
  lazy val instance = new ApiRateLimiter()
  def defaultRateLimit = 10
  def rateLimitInterval = 10000 // 10 seconds in ms
  val barInterval = 300000 // 5 minutes in ms
}

class ApiRateLimiter() {
  case class ApiHistory(lastBarTs: Long, history: Seq[Long])
  import ApiRateLimiter._
  // Some api key with rate limit
  val rateLimitByApiKey: Map[String, Int] = {
    val in = this.getClass.getResourceAsStream("/apiLimitdb.csv")
    Source.fromInputStream(in).getLines().drop(1)
      .map { row =>
        val cols = row.split(",")
        cols(0) -> cols(1).toInt
      }.toMap
  }

  def rateLimit(apiKey: String) = rateLimitByApiKey.getOrElse(apiKey, defaultRateLimit)

  // Keep the map of apiKey and its history
  val apiKeyHistory: mutable.Map[String, ApiHistory] = mutable.Map()

  def withinLimit(apiKey: String): Option[String] = {
    val now = System.currentTimeMillis
    val currentHistory = apiKeyHistory.getOrElseUpdate(apiKey, ApiHistory(0, Seq[Long]()))

    // check whether still being barred
    if (now - currentHistory.lastBarTs > barInterval) {
      val newHistory =
        getHistoryWithinRateLimitInterval(
          now,
          currentHistory.history,
          Seq[Long]())
      if (newHistory.size >= rateLimit(apiKey)) {
        apiKeyHistory.update(apiKey, ApiHistory(now, newHistory))
        None
      } else {
        apiKeyHistory.update(apiKey, ApiHistory(0, now +: newHistory))
        Some(apiKey)
      }
    } else None
  }

  @tailrec
  private def getHistoryWithinRateLimitInterval(now: Long, history: Seq[Long], accumulator: Seq[Long]): Seq[Long] = {
    if (history.isEmpty) accumulator
    else if (now - history.head < rateLimitInterval)
      getHistoryWithinRateLimitInterval(now, history.tail, accumulator :+ history.head)
    else accumulator
  }
}
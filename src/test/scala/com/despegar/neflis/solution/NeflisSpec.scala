package com.despegar.neflis.solution

import org.scalatest.{FlatSpec, Matchers}

class NeflisSpec extends FlatSpec with Matchers {

  "User pepe" should "have seen estrella things" in new NeflisData {
    val pepe = User(Set(film), estrellaThings.seasons.flatMap(_.episodes).toSet)
    val hasSeen = Neflis.userHasSeen(pepe, estrellaThings)
    assert(hasSeen, "User should have seen estrella things")
  }

  "The series duration" should "be calculated by adding the duration of all of them episodes" in new NeflisData {
    val seriesDuration = Neflis.durationFor(estrellaThings)
    assert(seriesDuration == 300)
  }

  "The film duration" should "be calculated by itself duration" in new NeflisData {
    val filmDuration = Neflis.durationFor(film)
    assert(filmDuration == 130)
  }

  "A series" should "resolve the last episode correctly" in new NeflisData {
    val lastEpisode = Neflis.lastEpisodeFor(estrellaThings)
    lastEpisode shouldBe Some(defaultEpisode.copy(order = 2))
  }

}

//Fixture
trait NeflisData {
  val genre = "SciFi"
  val actors = List("Millie Bobby Brown")
  val defaultEpisode = Episode(60, 1, genre, actors, Nil)
  val season1 = Season(List(defaultEpisode, defaultEpisode.copy(order = 2), defaultEpisode.copy(order = 3)), 1, actors, genre)
  val season2 = Season(List(defaultEpisode, defaultEpisode.copy(order = 2)), 1, actors, genre)
  val estrellaThings = Series(List(season1, season2), genre, actors)

  val film = Film("Thor", 130, List("Chris Hemsworth", "Tom Hiddleston"), "Action")
}
package com.despegar.neflis.solution

import org.scalatest.{FlatSpec, Matchers}

class NeflisSpec extends FlatSpec with Matchers {

  "User pepe" should "have seen estrella things - Series" in new NeflisData {
    val pepe = User(Set(thor), estrellaThings.seasons.flatMap(_.episodes).toSet)
    val hasSeen = Neflis.userHasSeen(pepe, estrellaThings)
    hasSeen shouldBe true
  }

  "User pepa" should "have not seen all episodes of estrella things - Series" in new NeflisData {
    val pepa = User(Set(thor), estrellaThings.seasons.head.episodes.toSet)
    val hasSeen = Neflis.userHasSeen(pepa, estrellaThings)
    hasSeen shouldBe false
  }

  "User pepi" should "have seen Thor - Film" in new NeflisData {
    val pepi = User(Set(thor), Set())
    val hasSeen = Neflis.userHasSeen(pepi, thor)
    hasSeen shouldBe true
  }

  "User pepo" should "have seen episode one of estrella things - Episode" in new NeflisData {
    val pepo = User(Set(thor), estrellaThings.seasons.head.episodes.toSet)
    val hasSeen = Neflis.userHasSeen(pepo, season1.episodes.head)
    hasSeen shouldBe true
  }

  "User pepo's brother" should "have seen season two of estrella things - Season" in new NeflisData {
    val pepon = User(Set(thor), estrellaThings.seasons.flatMap(_.episodes).toSet)
    val hasSeen = Neflis.userHasSeen(pepon, season2)
    hasSeen shouldBe true
  }

  "User pepu" should "is too busy to watch any content - Film and Series" in new NeflisData {
    val pepu = User(Set(), Set())
    val (hasSeenFilm, hasSeenSeries) = (Neflis.userHasSeen(pepu, thor), Neflis.userHasSeen(pepu, estrellaThings))
    hasSeenFilm shouldBe false
    hasSeenSeries shouldBe false
  }

}

//Fixture
trait NeflisData {
  val genre = "SciFi"
  val actors = List("Millie Bobby Brown")
  val defaultEpisode = Episode(60, 1, genre, actors, Nil)
  val season1 = Season(List(defaultEpisode, defaultEpisode.copy(order = 2), defaultEpisode.copy(order = 3)), 1, actors, genre)
  val season2 = Season(List(defaultEpisode.copy(duration = 59), defaultEpisode.copy(order = 2, duration = 59)), 1, actors, genre)
  val estrellaThings = Series(List(season1, season2), genre, actors)

  val thor = Film("Thor", 130, List("Chris Hemsworth", "Tom Hiddleston"), "Action")
}
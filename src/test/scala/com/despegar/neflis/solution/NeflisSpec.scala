package com.despegar.neflis.solution

import com.despegar.neflis.solution.NeflisData.{estrellaThings, film}
import org.scalatest.{FunSuite, Matchers}

class NeflisSpec extends FunSuite with Matchers {


  test("userHasSeen") {
    val pepe = User(Set(film), estrellaThings.seasons.flatMap(_.episodes).toSet)
    val hasSeen = Neflis.userHasSeen(pepe, estrellaThings)
    assert(hasSeen, "User should have seen estrella things")
  }
}

object NeflisData {
  val genre = "SciFi"
  val actors = List("Millie Bobby Brown")
  val defaultEpisode = Episode(60, 1, genre, actors, Nil)
  val season1 = Season(List(defaultEpisode, defaultEpisode.copy(order = 2), defaultEpisode.copy(order = 3)), 1, actors, genre)
  val season2 = Season(List(defaultEpisode, defaultEpisode.copy(order = 2)), 1, actors, genre)
  val estrellaThings = Series(List(season1, season2), genre, actors)

  val film = Film("Thor", 130, List("Chris Hemsworth", "Tom Hiddleston"), "Action")
}
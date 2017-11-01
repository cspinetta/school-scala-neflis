package com.despegar.neflis.solution

case class Series(seasons: List[Season], genre: String, protagonists: List[String]) extends Content {
  lazy val duration: Int = seasons.map(_.duration).sum

  def addEpisode(season: Season, episode: Episode): Either[String, Series] = {
    def addEpisode(season: Season, episode: Episode): Season = season.copy(episodes = season.episodes :+ episode)
    def replaceSeason(season: Season, newSeason: Season): Series = {
      this.copy(seasons = this.seasons.filterNot(_ == season) :+ newSeason)
    }

    this
      .seasons
      .find(_ == season)
      .toRight(s"Season doesn't exists in this series: $this")
      .flatMap(season => {
        val lastOrder = season.lastEpisode.map(_.order).getOrElse(0)
        if (lastOrder + 1 == episode.order) {
          val newSeason = addEpisode(season, episode)
          Right(replaceSeason(season, newSeason))
        } else {
          Left(s"The episode doesn't have a correct number order. The last episode " +
            s"is $lastOrder and the new episode is ${episode.order} instead of ${lastOrder + 1}")
        }
      })
  }
}

case class Season(episodes: List[Episode], number: Int, protagonists: List[String], genre: String) extends Content {
  lazy val countEpisodes: Int = episodes.size
  lazy val duration: Int = episodes.map(_.duration).sum

  lazy val lastEpisode: Option[Episode] = episodes.sortWith(_.order > _.order).headOption
}

case class Episode(duration: Int, order: Int, genre: String, protagonists: List[String], invitedActors: List[String]) extends Content

case class Film(title: String, duration: Int, protagonists: List[String], genre: String) extends Content

sealed trait Content {
  def genre: String
  def duration: Int
  def protagonists: List[String]
}

case class User(films: Set[Film], episodes: Set[Episode])

object Neflis {

  // Point 1: check if this [user] has seen this [content]
  def userHasSeen(user: User, content: Content): Boolean = content match {
    case Series(seasons, _, _) =>
      val shouldSee = seasons.flatMap(_.episodes)
      shouldSee.forall(user.episodes.contains)
    case Season(episodes, _, _, _) => episodes.forall(user.episodes.contains)
    case episode: Episode => user.episodes.contains(episode)
    case film: Film => user.films.contains(film)
  }

  // Point 2: Duration for a content
  def durationFor(content: Content): Int = content.duration

  // Point 3: Last episode for a series
  def lastEpisodeFor(series: Series): Option[Episode] = {
    series.seasons
      .sortBy(_.number)
      .lastOption
      .flatMap(_.episodes.sortBy(_.order).lastOption)
  }

  // The same with for-comprehension:
  def lastEpisodeForViaForComprehension(series: Series): Option[Episode] = {
    for {
      lastSeason <- series.seasons.sortBy(_.number).lastOption
      lastEpisode <- lastSeason.episodes.sortBy(_.order).lastOption
    } yield lastEpisode
  }

  // Point 4: some asks to user
  // Point 4.a: all genres seen by a user
  def genresSeenByUser(user: User): Set[String] = {
    user.episodes.map(_.genre) ++ user.films.map(_.genre)
  }

  // Point 4.b: favorite genre seen by a user
  def favoriteGenreByUser(user: User): Option[String] = {

    @scala.annotation.tailrec
    def computeGenreSeen(contents: List[Content], accumulated: Map[String, Int]): Map[String, Int] = contents match {
      case content :: otherContents =>
        val newAcc = accumulated + (content.genre -> (accumulated.getOrElse(content.genre, 0) + content.duration))
        computeGenreSeen(otherContents, newAcc)
      case Nil => accumulated
    }

    val genresSeenMap = computeGenreSeen((user.films ++ user.episodes).toList, Map.empty[String, Int])

    genresSeenMap.toList.sortWith(_._2 > _._2).headOption.map(_._1)
  }

  // Point 5.a: actor has acted on a given content
  def actorHasActedOn(actor: String, content: Content): Boolean = content.protagonists.contains(actor)

  // Point 5.b: actor has acted on a given content
  def userIsAFanOf(user: User, actor: String): Boolean = (user.films ++ user.episodes).forall(_.protagonists.contains(actor))

  // Point 6: Add a new episode
  def addEpisode(series: Series, season: Season, episode: Episode): Either[String, Series] = series.addEpisode(season, episode)

  // Point 7: happy rate ...
  def happyRate(user: User): Int = {
//      La película “Volver al Futuro” suma 150 puntos.
//      Cualquier contenido donde solo actúa Morgan Freeman son 100 puntos.
//      Cualquier pelicula donde actue Jack Nicholson son 80 puntos
//      Los capítulos con más actores invitados que protagonistas suman 80 puntos.
//      Las comedias (ya sean peliculas o capitulos) son 50 puntos
//      Los documentales que duran más de 2 horas son 20 puntos.
//      Los capítulos de terror que duran menos de 15 minutos son 15 puntos.
//      El resto del contenido suma 0 puntos.
    val scoreList = (user.films ++ user.episodes) map {
      case Film("Volver al Futuro", _, _, _) => 150
      case x: Content if x.protagonists.size == 1 && x.protagonists.contains("Morgan Freeman") => 100
      case Film(_, _, protagonists, _) if protagonists.contains("Jack Nicholson") => 80
      case Episode(_, _, _, protagonists, inviteds) if inviteds.size > protagonists.size => 80
      case x: Content if x.genre.equalsIgnoreCase("comedia") => 50
      case x: Content if x.genre.equalsIgnoreCase("documental") && x.duration > 60 => 20
      case Episode(duration, _, genre, _, _) if genre.equalsIgnoreCase("terror") && duration < 15 => 15
      case _ => 0
    }
    scoreList.sum
  }

}

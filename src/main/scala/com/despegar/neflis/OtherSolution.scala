package com.despegar.neflis

trait Contenido {
 def genero:String
 def duracion:Int
 def actuo(actor:String):Boolean
}

case class Pelicula(actores:List[String], duracion:Int, genero:String) extends Contenido {
 def fueVistoCompletoPor(usuario: Usuario):Boolean = usuario.vio(this)
 def actuo(actor:String):Boolean = actores.contains(actor)
}

case class Serie(protagonistas:List[String], temporadas:List[Temporada], genero:String) extends Contenido {

 def fueVistoCompletoPor(usuario: Usuario) = temporadas.forall(_.fueVistoCompletoPor(usuario))

 def duracion = temporadas.map(_.duracion).sum

 def ultimoCapituloDisponible = ultimaTemporadaDisponible.flatMap(_.ultimoCapituloDisponible)

 def ultimaTemporadaDisponible = temporadas.headOption

 def actuo(actor:String) = temporadas.exists(_.actuo(actor))

 def agregaTemporada() = copy (
   temporadas = Temporada(temporadas.length + 1, genero) :: temporadas
 )
 def agregaCapitulo(duracion: Int, invitados:List[String]) = temporadas match {
   case current :: oldest => copy(temporadas = current.agregarCapitulo(duracion, protagonistas, invitados) :: oldest)
   case Nil => throw new RuntimeException("Empty Temporadas")
 }

}
object Serie {
 def apply(protagonistas: List[String], genero: String): Serie = new Serie(protagonistas, Nil, genero)
}

case class Temporada(numero:Int, capitulos:List[Capitulo], genero:String) {

 def fueVistoCompletoPor(usuario: Usuario) = capitulos.forall(_.fueVistoCompletoPor(usuario))

 def duracion = capitulos.map(_.duracion).sum

 def ultimoCapituloDisponible = capitulos.sortBy(-_.numero).headOption

 def actuo(actor:String) = capitulos.exists(_.actuo(actor))

 def agregarCapitulo(duracion:Int, protagonistas:List[String], invitados:List[String]) = copy (
   capitulos = Capitulo(capitulos.length + 1, duracion, protagonistas, invitados, genero) :: capitulos
 )

}
object Temporada {
 def apply(numero: Int, genero: String): Temporada = new Temporada(numero, Nil, genero)
}


case class Capitulo(numero:Int, duracion:Int, protagonistas:List[String], invitados:List[String], genero: String) extends Contenido {

 def fueVistoCompletoPor(usuario: Usuario) = usuario.vio(this)

 def esProtagonista(actor:String) = protagonistas.contains(actor)

 def esInvitado(actor:String) = invitados.contains(actor)

 def actuo(actor:String) = esProtagonista(actor) || esInvitado(actor)

}

case class Usuario(contenidoVisto:List[Contenido]) {

 def vio(contenido: Contenido) = contenidoVisto.contains(contenido)

 def generosVistos:Set[String] = contenidoVisto.map(_.genero).toSet

 def generoFavorito:String = generosVistos.maxBy( g => contenidoVisto.filter(_.genero == g).map(_.duracion).sum )

 def esFan(actor:String) = contenidoVisto.forall(_.actuo(actor))

}

object Main extends App {

 val protagonistas = "Homero" :: "Bart" :: "Marge" :: "Lisa" :: "Maggie" :: Nil

 val simpsons = Serie(protagonistas, "comedia")
   .agregaTemporada()
   .agregaCapitulo(60, "Burns" :: Nil)
   .agregaCapitulo(60, "Krusty" :: Nil)
   .agregaCapitulo(60, "Nelson" :: Nil)
   .agregaTemporada()
   .agregaCapitulo(60, "Bob" :: Nil)

 println(simpsons.actuo("Bob"))

val protagonistasIt = "Bill" :: "Pennywise" :: "Ben" :: Nil
 val protagonistasTheRing = "Samara" :: "Rachel" :: "Clay" :: Nil

 val it = Pelicula(protagonistasIt,120, "terror")
 val theRing = Pelicula(protagonistasTheRing, 124, "terror")

 Seq(simpsons, it, theRing).foreach {
  case p: Pelicula => println(s"Pelicula de ${p.genero}")
  case s: Serie => println(s"Serie de ${s.genero}")
 }



}



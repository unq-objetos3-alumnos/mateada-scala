class Mate(_ronda: Ronda) {
  var _cebado = true

  def cebado = _cebado

  def cuantasVecesTome(mateador: Mateador) = _ronda.cuantasVecesTomo(mateador)

  def recibidoPor(mateador: Mateador) = _ronda.recibio(mateador)

  def yaTomo(mateador: Mateador) = _ronda.yaTomo(mateador)

  def tomo(mateador: Mateador) = {
    _cebado = false
    _ronda.tomo(mateador)
  }
}

class Cadena(var _siguientes: List[Mateador]) {

  def pasar(mate: Mate): Unit = {
    if (_siguientes.nonEmpty) {
      val siguiente :: resto = _siguientes
      _siguientes = resto
      siguiente.recibirMate(mate, this)
    }
  }

}

class Ronda(_mateadores: List[Mateador]) {

  var _recibieron: List[Mateador] = List()
  var _tomaron: Map[Mateador, Int] = Map()

  def iniciarPasada(mate: Mate) = {
    val cadena = new Cadena(_mateadores)
    cadena.pasar(mate)
  }

  def yaTomo(mateador: Mateador) = _tomaron.contains(mateador)

  def tomo(mateador: Mateador) = _tomaron += (mateador -> (_tomaron.getOrElse(mateador, 0) + 1))

  def cuantasVecesTomo(mateador: Mateador) = _tomaron.getOrElse(mateador, 0)

  def recibio(mateador: Mateador) = _recibieron ::= mateador

  def todosRecibieronMate() = _recibieron.size == _mateadores.size
}

class Cebador(_mateadores: List[Mateador]) {

  def darRonda() = {
    val ronda = new Ronda(_mateadores)
    while (!ronda.todosRecibieronMate()) {
      ronda.iniciarPasada(new Mate(ronda))
    }
  }

}

class Mateador() {
  var _cuantosTomaste = 0
  var _cuantosRecibiste = 0

  def cuantosTomaste = _cuantosTomaste

  def cuantosRecibiste = _cuantosRecibiste

  def recibirMate(mate: Mate, cadena: Cadena) = {
    if (mate.yaTomo(this)) {
      noEsMiTurno(mate, cadena)
    } else {
      if (mate.cebado) {
        _cuantosRecibiste += 1
        mate.recibidoPor(this)
        tomarMate(mate, cadena)
      }
    }
  }

  protected def tomarMate(mate: Mate, cadena: Cadena) = {
    _cuantosTomaste += 1
    mate.tomo(this)
  }

  protected def noEsMiTurno(mate: Mate, cadena: Cadena) = {
    cadena.pasar(mate)
  }
}

class Simulador() extends Mateador() {
  override def tomarMate(mate: Mate, cadena: Cadena) = {
    cadena.pasar(mate)
  }
}

class Garca() extends Mateador() {
  override def noEsMiTurno(mate: Mate, cadena: Cadena) = {
    if (mate.cuantasVecesTome(this) == 1) {
      tomarMate(mate, cadena)
    }
    super.noEsMiTurno(mate, cadena)
  }
}

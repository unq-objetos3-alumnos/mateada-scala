import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class MateadaTest extends AnyFunSpec with Matchers {
  def todosTomaron(cantidad: Int, mateadores: List[Mateador]) =
    mateadores.forall(mateador => mateador.cuantosTomaste == cantidad) should be(true)

  def todosRecibieron(cantidad: Int, mateadores: List[Mateador]) =
    mateadores.forall(mateador => mateador.cuantosRecibiste == cantidad) should be(true)

  describe("Rondas con personas simples") {
    it("un mateador arranca sin haber tomado ningún mate") {
      val mateador = new Mateador()
      mateador.cuantosTomaste should equal(0)
    }

    it("debe dar una única ronda y el unico mateador toma") {
      val mateador = new Mateador()
      val cebador = new Cebador(List(mateador))

      cebador.darRonda()

      mateador.cuantosTomaste should equal(1)
    }

    it("debe dar una única ronda y todos los mateadores toman 1 mate") {
      val mateadores = List(new Mateador(), new Mateador(), new Mateador())
      val cebador = new Cebador(mateadores)

      cebador.darRonda()

      todosTomaron(1, mateadores)
    }

    it("debe dar dos rondas con tres mateadores y todos deben haber tomado dos mates") {
      val mateadores = List(new Mateador(), new Mateador(), new Mateador())
      val cebador = new Cebador(mateadores)

      cebador.darRonda()
      cebador.darRonda()

      todosTomaron(2, mateadores)
    }

    it("una ronda con 2 mateadores y un simulador, todos reciben un mate, todos toman menos el simulador") {
      val simulador = new Simulador()
      val mateador1 = new Mateador()
      val mateador2 = new Mateador()
      val mateadores = List(mateador1, simulador, mateador2)
      val cebador = new Cebador(mateadores)

      cebador.darRonda()

      todosRecibieron(1, mateadores)
      todosTomaron(1, List(mateador1, mateador2))
      simulador.cuantosTomaste should equal(0)
    }

    it("un garca toma 2 veces aunque no sea su turno") {
      val garca = new Garca()
      val mateador1 = new Mateador()
      val mateador2 = new Mateador()
      val mateadores = List(garca, mateador1, mateador2)
      val cebador = new Cebador(mateadores)

      cebador.darRonda()

      todosRecibieron(1, mateadores)
      todosTomaron(1, List(mateador1, mateador2))
      garca.cuantosTomaste should equal(2)
    }
  }
}

package peterlavalle

trait TTest {

	private lazy val startup: Long = System.currentTimeMillis()

	def benchmark[O](name: String)(action: => O): O = {
		val start: Double = age()
		println(s"[$name] ; starting ...")
		val out = action
		val total: Double = age() - start
		println(s"[$name] ; ... done ($total)")
		out
	}

	protected def age(): Double =
		(System.currentTimeMillis() - startup) * 0.001
}

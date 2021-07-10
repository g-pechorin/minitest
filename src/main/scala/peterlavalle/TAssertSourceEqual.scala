package peterlavalle

import java.io.File

import org.scalatest.funsuite.AnyFunSuite

trait TAssertSourceEqual {

	this: AnyFunSuite with TemplateResource =>

	private lazy val target: File = new File("target").getAbsoluteFile

	def assertReSourceEqual(expected: String)(actual: Seq[String]): Unit = {
		assertSourceEqual(resourceStream(expected), actual)
	}

	def assertSourceEqual(expected: Seq[String], actual: Seq[String]): Unit =
		assertSourceEqual(
			expected.foldLeft("")((_: String) + (_: String) + "\n"),
			actual.foldLeft("")((_: String) + (_: String) + "\n"),
		)

	def assertSourceEqual(expected: String, actual: String): Unit = {
		def normalise: String => String =
			normalisers.reverse.foldRight(_: String)(_ apply _)

		lazy val traceCode: String =
			getClass.getName + Math.abs(
				Thread.currentThread()
					.getStackTrace
					.toList
					.foldLeft("")((_: String) + (_: StackTraceElement))
					.hashCode
			).toString


		val ev: String = normalise(expected)
		val av: String = normalise(actual)

		val ef: File =
			target / (traceCode + ".expected") ioWriteLines expected

		val af: File =
			target / (traceCode + ".actual") ioWriteLines actual

		if (ev != av) {
			System.err.println(
				"kdiff3" +
					" " +
					ef.AbsolutePath +
					" " +
					af.AbsolutePath
			)
			assert(av == ev)
		}
	}

	def normalisers =
		List(
			(_: String).replaceAll("([\t \r]*\n)+", "\n"),
			(_: String).trim
		)

	def resourceString(name: String): String =
		resourceStream(name).foldLeft("")(_ + _ + "\n")

	def resourceStream(name: String): Stream[String] = {
		bind(name) {
			name =>
				fail(s"there should be no templates, but, asked for $name")
		}
	}
}

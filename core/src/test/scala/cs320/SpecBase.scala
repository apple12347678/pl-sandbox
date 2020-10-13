package cs320

import org.scalatest.flatspec.AnyFlatSpec

trait SpecBase extends AnyFlatSpec {

  val len = 70

  def normalize(s: String): String = {
    val s1 = s.replaceAll("Spec.", "")
      .replaceAll("Implementation.", "")
      .replaceAll("Predef.", "")
      .replaceAll(".apply", "")
    if (s1.length <= len) s1 else s"${s1.substring(0, len)} ..."
  }
}

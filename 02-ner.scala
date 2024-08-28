// # Named Entity Recognition
//
// In this section we'll use rewrite systems to perform [named entity
// recognition](https://en.wikipedia.org/wiki/Named-entity_recognition). This is
// a natural language processing task where we attempt to extract particular
// items (entities) of interest from text. In our case we're interested in
// extracting numeric entities, such as number and dates.
@main def ner(): Unit = {
  // In the previous example we worked on an expression *tree*. Text, on the
  // other hand, has a linear structure (unless we first parse it into a parse
  // tree, but that's a very hard problem we wish to avoid.) This means our
  // rewrites will also have a different structure. We will define rewrites as
  // transformations from a list of entities to a list of entities, where an
  // entity is either a character or some more structured data. We also have
  // some restrictions on rewrites:
  //
  // 1. a rewrite may match any number of entities within the list, but those
  // must all be consecutive elements and start at the start of the list
  //
  // 2. if the rewrite matches it must rewrite the elements it matches to a
  // single element at the start of its result, and leave the rest of the list
  // alone.
  //
  // These two criteria allow us to ensure we make progress in rewriting and
  // don't endlessly loop. The criteria are bit imprecisely expressed above.
  // They will be clearer with examples.
  //
  // Let's define `Rewrite`
  trait Rewrite[A] {
    def apply(in: List[A | Char]): Option[List[A | Char]]

    def orElse(that: Rewrite[A]): Rewrite[A] = {
      val self = this
      new Rewrite {
        def apply(in: List[A | Char]): Option[List[A | Char]] =
          self(in).orElse(that(in))
      }
    }
  }
  object Rewrite {
    def lift[A](
        f: PartialFunction[List[A | Char], List[A | Char]]
    ): Rewrite[A] =
      new Rewrite {
        val lifted = f.lift
        def apply(in: List[A | Char]): Option[List[A | Char]] =
          lifted(in)
      }
  }

  // Now let's define entities that we with to extract. For simplicity we will
  // only look for integers for now.
  enum Entity {
    case Number(value: Int)
  }
  import Entity.*

  // The simplest rule is to find single digit numbers.
  val singleDigit: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case '0' :: rest => Number(0) :: rest }
      .orElse(
        Rewrite.lift { case '1' :: rest => Number(1) :: rest }
      )
      .orElse(
        Rewrite.lift { case '2' :: rest => Number(2) :: rest }
      )
      .orElse(
        Rewrite.lift { case '3' :: rest => Number(3) :: rest }
      )
      .orElse(
        Rewrite.lift { case '4' :: rest => Number(4) :: rest }
      )
      .orElse(
        Rewrite.lift { case '5' :: rest => Number(5) :: rest }
      )
      .orElse(
        Rewrite.lift { case '6' :: rest => Number(6) :: rest }
      )
      .orElse(
        Rewrite.lift { case '7' :: rest => Number(7) :: rest }
      )
      .orElse(
        Rewrite.lift { case '8' :: rest => Number(8) :: rest }
      )
      .orElse(
        Rewrite.lift { case '9' :: rest => Number(9) :: rest }
      )

  // Now we need a method to apply rewrites
  def rewrite[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] =
    text match {
      case head :: rest =>
        rule(text) match {
          case None               => head :: rewrite(rest, rule)
          case Some(head :: rest) =>
            // The conditions on rewrites ensure that if they match, the head is
            // the rewritten result and rest is the unmatched remainder that we
            // should attempt to process.
            head :: rewrite(rest, rule)
          case Some(Nil) =>
            // This case can't happen but need it to make exhaustiveness
            // checking happy (and to catch buggy rules!)
            throw new Exception(
              s"A Rewrite rule returned Nil on input $text, which should never happen."
            )
        }

      case Nil => Nil
    }

  // Let's test our rules on some input
  println(rewrite("1 is something we should find".toList, singleDigit))
  println(rewrite("Do we find this 1 as well?".toList, singleDigit))
  println(
    rewrite(
      "12 is something we don't quite handle properly".toList,
      singleDigit
    )
  )

  // We find single digit numbers just fine, but if we have a two or more digit
  // number we extract it as several single digit numbers. We need to coallesce
  // consecutive single digit numbers into a single number. How can we do this?
  // With a rewrite rule!
  val multiDigit: Rewrite[Entity] =
    Rewrite.lift { case Number(a) :: Number(b) :: rest =>
      Number(a * 10 + b) :: rest
    }

  // Now we see why we have restrictions on rewrites. If we didn't have them we
  // couldn't safely write this rule.
  //
  // Does it work, though? Well, we need to apply the rules multiple times to
  // see it taking effect, so we need to implement a method that does this.
  def iterate[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] = {
    println(text)

    val t = rewrite(text, rule)
    if t == text then text
    else iterate(t, rule)
  }

  println(
    iterate(
      "Is 12 something we handle properly?".toList,
      singleDigit.orElse(multiDigit)
    )
  )

  println(
    iterate(
      "Can we count to 112? What about 1012?".toList,
      singleDigit.orElse(multiDigit)
    )
  )

  // Hmmm ... it works for 12 and 112, but not 1012. The trace of the rewrites
  // shows why: we combine two pairs 1 and 0, and 1 and 2 to get 10 and 12, then
  // compute (10 * 10) + 12 when we really want (10 * 100) + 12.
  //
  // Let's correct the implementation. The number of digits in the right hand
  // side number tells us how many zeros we need to add to the left hand side
  // number. We can compute this by taking the floor or the log base 10 of the
  // right hand side number.
  def log10(x: Int) = Math.log(x.toDouble) / Math.log(10)

  val multiDigitCorrect: Rewrite[Entity] =
    Rewrite.lift { case Number(a) :: Number(b) :: rest =>
      // Don't try to compute the log of 0; it's not defined!
      if b < 10 then Number(a * 10 + b) :: rest
      else
        Number(
          a * Math.pow(10, Math.floor(log10(b)) + 1).toInt + b
        ) :: rest
    }

  // Better test it!
  println(iterate("1012".toList, singleDigit.orElse(multiDigitCorrect)))
  println(iterate("65535".toList, singleDigit.orElse(multiDigitCorrect)))
  println(iterate("262144".toList, singleDigit.orElse(multiDigitCorrect)))
  println(iterate("16777215".toList, singleDigit.orElse(multiDigitCorrect)))

  // It works!
}

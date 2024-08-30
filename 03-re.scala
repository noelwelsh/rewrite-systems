// # Regular Expressions on Lists
@main def re(): Unit = {
  enum Regexp[A] {
    def ++(that: Regexp[A]): Regexp[A] =
      Append(this, that)

    def orElse(that: Regexp[A]): Regexp[A] =
      OrElse(this, that)

    def repeat: Regexp[A] =
      Repeat(this)

    def `*`: Regexp[A] = this.repeat
    def `+`: Regexp[A] = this ++ (this.repeat)

    def matches[AA >: A](input: List[AA]): Option[(List[A], List[AA])] =
      this match {
        case Append(left, right) =>
          left.matches(input) match {
            case None => None

            case Some(matched, rest) =>
              right.matches(rest) match {
                case None => None
                case Some(matched2, rest2) =>
                  Some(matched ++ matched2, rest2)
              }
          }

        case OrElse(first, second) =>
          // This is not quite correct, but probably good enough for us. See the
          // book for more.
          first.matches(input).orElse(second.matches(input))

        case Repeat(source) =>
          def loop(matched: List[A], rest: List[AA]): Option[(List[A], List[AA])] =
            rest match {
              case Nil => Some((matched, rest))
              case other =>
                source.matches(other) match {
                  case None => Some((matched, rest))
                  case Some((matched2, rest2)) => loop(matched ++ matched2, rest2)
                }
            }

          loop(List.empty, input)

        case Apply(elts) =>
          def loop(find: List[A], rest: List[AA]): Option[(List[A], List[AA])] =
            if find.isEmpty then Some(elts, rest)
            else {
              val a = find.head

              if rest.isEmpty then None
              else {
                val h = rest.head
                if a == h then loop(find.tail, rest.tail)
                else None
              }
            }

          loop(elts, input)

        case Refine(f) =>
          input match {
            case Nil => None
            case a :: rest =>
              f(a).map(a => (List(a), rest))
          }

        case Empty() => Some(Nil, input)
      }

    case Append(left: Regexp[A], right: Regexp[A])
    case OrElse(first: Regexp[A], second: Regexp[A])
    case Repeat(source: Regexp[A])
    case Apply(elts: List[A])
    case Refine(f: Any => Option[A])
    case Empty()
  }
  object Regexp {
    def empty[A]: Regexp[A] = Empty()

    def apply[A](elt: A, elts: A*): Regexp[A] =
      Apply((elt +: elts).toList)

    def refine[A](f: Any => Option[A]): Regexp[A] =
       Refine(f)
  }



  trait Rewrite[A] {
    // On success, the first list is the rewritten content, the second list is
    // the content remaining to be rewritten.
    def apply(in: List[A | Char]): Option[(List[A], List[A | Char])]

    def orElse(that: Rewrite[A]): Rewrite[A] = {
      val self = this
      new Rewrite {
        def apply(in: List[A | Char]): Option[(List[A], List[A | Char])] =
          self(in).orElse(that(in))
      }
    }
  }
  object Rewrite {
    def lift[A](
        f: PartialFunction[List[A | Char], (List[A], List[A | Char])]
    ): Rewrite[A] =
      new Rewrite {
        val lifted = f.lift
        def apply(in: List[A | Char]): Option[(List[A], List[A | Char])] =
          lifted(in)
      }

    def lift[A, AA >: A](re: Regexp[A])(f: List[A] => List[AA]): Rewrite[AA] =
      new Rewrite {
        def apply(in: List[AA | Char]): Option[(List[AA], List[AA | Char])] =
          re.matches(in) match {
            case None => None
            case Some((matched, rest)) => Some((f(matched), rest))
          }
      }
  }


  enum Entity {
    case NumberBlock(digits: List[Digit])
    case Number(value: Int)
    case Digit(value: Int)
  }
  import Entity.*


  val digitRe = Regexp.refine[Digit]{
    case Digit(d) => Some(Digit(d))
    case _ => None
  }

  val digit: Rewrite[Entity] =
    Rewrite
      .lift[Entity] { case '0' :: rest => (List(Digit(0)), rest) }
      .orElse(
        Rewrite.lift { case '1' :: rest => (List(Digit(1)), rest) }
      )
      .orElse(
        Rewrite.lift { case '2' :: rest => (List(Digit(2)), rest) }
      )
      .orElse(
        Rewrite.lift { case '3' :: rest => (List(Digit(3)), rest) }
      )
      .orElse(
        Rewrite.lift { case '4' :: rest => (List(Digit(4)), rest) }
      )
      .orElse(
        Rewrite.lift { case '5' :: rest => (List(Digit(5)), rest) }
      )
      .orElse(
        Rewrite.lift { case '6' :: rest => (List(Digit(6)), rest) }
      )
      .orElse(
        Rewrite.lift { case '7' :: rest => (List(Digit(7)), rest) }
      )
      .orElse(
        Rewrite.lift { case '8' :: rest => (List(Digit(8)), rest) }
      )
      .orElse(
        Rewrite.lift { case '9' :: rest => (List(Digit(9)), rest) }
      )

  val createNumberBlock: Rewrite[Entity] =
    // We can't prove to the type checker that digits is always a List[Digit], but
    // it must be beause digitRe will only match in this condition. Hence we use
    // asInstanceOf. Don't tell anyone!
    Rewrite.lift[Digit, Entity](digitRe.+){
      case Digit(d) :: digits => List(Number(d), NumberBlock(digits))
      case Nil => throw new Exception("Regexp matched 0 elements. This should not happen.")
    }

  val splitNumberBlock: Rewrite[Entity] =
    Rewrite.lift{ case Number(n) :: NumberBlock(Digit(d) :: digits) :: rest =>
      (List(Number(n * 10 + d), NumberBlock(digits)), rest)
    }

  val removeNumberBlock: Rewrite[Entity] =
    Rewrite.lift{ case NumberBlock(Nil) :: rest => (Nil, rest) }

  val rule: Rewrite[Entity] =
    digit.orElse(createNumberBlock).orElse(splitNumberBlock).orElse(removeNumberBlock)

  def rewrite[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] =
    text match {
      case head :: rest =>
        rule(text) match {
          case None                    => head :: rewrite(rest, rule)
          case Some((rewritten, rest)) => rewritten ++ rewrite(rest, rule)
        }

      case Nil => Nil
    }

  def iterate[A](text: List[Char | A], rule: Rewrite[A]): List[Char | A] = {
    println(text)

    val t = rewrite(text, rule)
    if t == text then text
    else iterate(t, rule)
  }

  println(iterate("16777215".toList, rule))
}

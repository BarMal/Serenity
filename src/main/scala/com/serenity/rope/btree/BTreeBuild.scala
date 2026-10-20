package com.serenity.rope.btree

import com.serenity.rope.{Balance, Branch, Leaf, Rope}

/** Builds trees bottom-up from nodes already in hand. Every tree it returns keeps all leaves at one depth. */
private[rope] object BTreeBuild:

  /** A tree over LF-only `text`, its leaves filled to the target size. */
  def fromText(text: String)(using balance: Balance): Rope =
    assemble(Chunking.bulkPieces(text, LeafLimits.of(balance)).map(piece => Leaf(piece)))

  /** A tree over the non-empty `values`, each kept as the string it is unless it exceeds the largest leaf. */
  def fromLeafValues(values: Iterable[String])(using balance: Balance): Rope =
    val limits = LeafLimits.of(balance)
    assemble(values.iterator.filter(_.nonEmpty).flatMap(Chunking.pieces(_, limits)).map(piece => Leaf(piece)).toVector)

  /** A tree over `nodes`, which must all be the same height, in order. Fewer than two need no parent; up to `Max` share
    * one; more are grouped into parents first, level by level, until one root is left.
    */
  def assemble(nodes: Vector[Rope])(using Balance): Rope =
    nodes.size match
      case 0                            => Leaf("")
      case 1                            => nodes(0)
      case count if count <= Fanout.Max => Branch(nodes)
      case _                            => assemble(group(nodes, Fanout.Target))

  /** `nodes` as parents of about `perParent` children each, never fewer than `Min` unless there are too few for two. */
  def group(nodes: Vector[Rope], perParent: Int)(using Balance): Vector[Rope] =
    val count = nodes.size
    if count <= Fanout.Max then Vector(Branch(nodes))
    else
      val parents                 = math.min(math.max(1, (count + perParent - 1) / perParent), count / Fanout.Min)
      val base                    = count / parents
      val extra                   = count % parents
      def start(parent: Int): Int = parent * base + math.min(parent, extra)
      Vector.tabulate(parents)(parent => Branch(nodes.slice(start(parent), start(parent + 1))))

package util.collections

import scala.collection.generic.CanBuildFrom

/** Miscellaneous convenience functions for working with scala collections */
object RichCollections {

  /** Miscellaneous convenience functions on collections of pairs */
  implicit class RichPairCollectionsOps[KEY, VALUE](collection: Traversable[(KEY, VALUE)]) {

    /** Folds over a pair collection.
     *
     * @param initial
     *   the start value
     * @param func
     *   the function to apply
     * @tparam OUT
     *   aggregation type
     * @return
     *   the result
     */
    def foldLeftByKey[OUT](initial: => OUT)(func: (OUT, VALUE) => OUT): Map[KEY, OUT] = {
      collection
        .groupBy(_._1)
        .map { case (key, value) =>
          key -> value.map(_._2).foldLeft(initial) { case (acc, value) => func(acc, value) }
        }
    }

    /** Reduces over a pair collection.
     *
     * @param func
     * the function to apply
     * @return
     * the result
     */
    def reduceByKey(func: (VALUE, VALUE) => VALUE): Map[KEY, VALUE] = {
      collection
        .groupBy(_._1)
        .map { case (key, value) =>
          key -> value.map(_._2).reduce(func)
        }
    }

    /** Takes a collection of pairs and aggregates the values by key into a collection of a user defined type.
     *
     * @param b1
     *   implicit builder
     * @tparam OUT
     *   aggregation type
     * @return
     *   the resul
     */
    def aggregatedByKey[OUT](implicit b1: CanBuildFrom[OUT, VALUE, OUT]): Map[KEY, OUT] =
      foldLeftByKey(b1()) { case (acc, value) => acc += value }
        .map { case (key, builder) => key -> builder.result() }
  }

}

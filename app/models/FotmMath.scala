package models

import models.Clusterer
import models.Clusterer._

import scala.util.Random

object FotmMath {
  def add(v1: V, v2: V) = v1.zip(v2).map(v => v._1 + v._2)

  def sub(v1: V, v2: V) = add(v1, v2.map(- _))

  def dist(v1: V, v2: V) = len(sub(v1, v2))

  def sqrdist(v1: V, v2: V) = sqrlen(sub(v1, v2))

  def div(v: V, a: Double) = v.map(_ / a)

  def scale(v: V, a: Double) = v.map(_ * a)

  def len(v: V) = Math.sqrt( sqrlen(v) )

  def sqrlen(v: V) = v.map(a => a * a).sum

  def uniformlyDistributedPoints(nPoints: Int): Seq[Clusterer.V] = {
    val rng = new Random()
    (1 to nPoints).map { i => Vector(rng.nextDouble, rng.nextDouble) }
  }

  def gaussianDistributedPoints(nPoints: Int, nSeeds: Int): Seq[Clusterer.V] = {
    val rng = new Random()
    val seeds = (1 to nSeeds).map { i => Vector(rng.nextDouble, rng.nextDouble) }
    for {
      s <- seeds
      groupSize <- (1 to nPoints / nSeeds)
    } yield scale(add(s, Vector(rng.nextGaussian * .05, rng.nextGaussian * .05)), 0.8)
  }
}

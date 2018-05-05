package undersampling.core

import undersampling.data.Data
import undersampling.util.Utilities._

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, sqrt}
import scala.util.Random

/** Evolutionary Under Sampling. Original paper: "Evolutionary Under-Sampling for Classification with Imbalanced Data
  * Sets: Proposals and Taxonomy" by Salvador Garcia and Francisco Herrera.
  *
  * @param data          data to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class EvolutionaryUnderSampling(override private[undersampling] val data: Data,
                                override private[undersampling] val seed: Long = System.currentTimeMillis(),
                                override private[undersampling] val minorityClass: Any = -1) extends Algorithm(data, seed, minorityClass) {

  /** Compute Evolutionary Under Sampling
    *
    * @param file           file to store the log. If its set to None, log process would not be done
    * @param populationSize number of chromosomes to generate
    * @param maxEvaluations number of evaluations
    * @param algorithm      version of algorithm to execute. One of: EBUSGSGM, EBUSMSGM, EBUSGSAUC, EBUSMSAUC,
    *                       EUSCMGSGM, EUSCMMSGM, EUSCMGSAUC or EUSCMMSAUC
    * @param distance       distance to use when calling the NNRule algorithm
    * @param probHUX        probability of changing a gen from 0 to 1 (used in crossover)
    * @param recombination  recombination threshold (used in reinitialization)
    * @param prob0to1       probability of changing a gen from 0 to 1 (used in reinitialization)
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, populationSize: Int = 50, maxEvaluations: Int = 10000,
             algorithm: String = "EBUSGSGM", distance: Distances.Distance = Distances.EUCLIDEAN, probHUX: Double = 0.25,
             recombination: Double = 0.35, prob0to1: Double = 0.05): Data = {
    // Use randomized data
    val dataToWorkWith: Array[Array[Double]] = (this.index map this.x).toArray
    // and randomized classes to match the randomized data
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    val initDistancesTime: Long = System.nanoTime()
    // Distances among the elements
    val distances: Array[Array[Double]] = computeDistances(dataToWorkWith, distance, this.data._nominal, this.y)
    val distancesTime: Long = System.nanoTime() - initDistancesTime

    val majoritySelection: Boolean = algorithm.contains("MS")
    val targetInstances: Array[Int] = if (majoritySelection) classesToWorkWith.zipWithIndex.collect { case (c, i)
      if c != this.untouchableClass => i
    } else classesToWorkWith.indices.toArray

    def fitnessFunction(instance: Array[Int]): Double = {
      val index: Array[Int] = zeroOneToIndex(instance) map targetInstances
      val predicted: Array[Any] = dataToWorkWith.indices.map((e: Int) => nnRule(distances = distances(e),
        selectedElements = index.diff(Array(e)), labels = classesToWorkWith, k = 1)._1).toArray

      val matrix: (Int, Int, Int, Int) = confusionMatrix(originalLabels = index map classesToWorkWith, predictedLabels = predicted, minorityClass = this.untouchableClass)

      val tp: Int = matrix._1
      val fp: Int = matrix._2
      val fn: Int = matrix._3
      val tn: Int = matrix._4

      val nPositives: Int = (index map classesToWorkWith).count((_: Any) == this.untouchableClass)
      val nNegatives: Int = (index map classesToWorkWith).length - nPositives

      val tpr: Double = tp / (tp + fn)
      val fpr: Double = fp / (fp + fn)

      val fitness: Double = if (algorithm.contains("GM")) {
        val tnr: Double = tn / (tn + fp)
        val g: Double = sqrt(tpr * tnr)

        if (algorithm == "EBUSGSGM") {
          g - abs(1 - (nPositives.toFloat / nNegatives)) * 20
        } else if (algorithm == "EBUSMSGM") {
          g - abs(1 - (this.counter(this.untouchableClass).toFloat / nNegatives)) * 20
        } else if (algorithm == "EUSCMGSGM") {
          g
        } else if (algorithm == "EUSCMMSGM") {
          g
        } else {
          Double.NaN
        }
      } else if (algorithm.contains("AUC")) {
        val auc: Double = (1.0 + tpr - fpr) / 2.0

        if (algorithm == "EBUSGSAUC") {
          auc - abs(1 - (nPositives.toFloat / nNegatives)) * 0.2
        } else if (algorithm == "EBUSMSAUC") {
          auc - abs(1 - (this.counter(this.untouchableClass).toFloat / nNegatives)) * 0.2
        } else if (algorithm == "EUSCMGSAUC") {
          auc
        } else if (algorithm == "EUSCMMSAUC") {
          auc
        } else {
          Double.NaN
        }
      } else {
        Double.NaN
      }

      if (fitness.isNaN)
        throw new Exception("Invalid argument: algorithm should be: EBUSGSGM, EBUSMSGM, EBUSGSAUC, EBUSMSAUC, EUSCMGSGM, EUSCMMSGM, EUSCMGSAUC or EUSCMMSAUC")

      fitness
    }

    val random: Random = new Random(this.seed)
    val population: Array[Array[Int]] = new Array[Array[Int]](populationSize)
    (0 until populationSize).foreach((i: Int) => population(i) = targetInstances.indices.map((_: Int) => random.nextInt(2)).toArray)

    val evaluations: Array[Double] = new Array[Double](population.length)
    population.zipWithIndex.par.foreach { (chromosome: (Array[Int], Int)) =>
      evaluations(chromosome._2) = fitnessFunction(chromosome._1)
    }

    var incestThreshold: Int = targetInstances.length / 4
    var actualEvaluations: Int = populationSize

    while (actualEvaluations < maxEvaluations) {
      val randomPopulation: Array[Array[Int]] = random.shuffle(population.indices.toList).toArray map population
      val newPopulation: ArrayBuffer[Array[Int]] = new ArrayBuffer[Array[Int]](0)

      (randomPopulation.indices by 2).foreach { i: Int =>
        val hammingDistance: Int = (randomPopulation(i) zip randomPopulation(i + 1)).count((pair: (Int, Int)) => pair._1 != pair._2)

        if ((hammingDistance / 2) > incestThreshold) {
          val desc1: Array[Int] = randomPopulation(i).clone
          val desc2: Array[Int] = randomPopulation(i + 1).clone

          desc1.indices.foreach { i: Int =>
            if (desc1(i) != desc2(i) && random.nextFloat < 0.5) {
              desc1(i) = if (desc1(i) == 1) 0 else if (random.nextFloat < probHUX) 1 else desc1(i)
              desc2(i) = if (desc2(i) == 1) 0 else if (random.nextFloat < probHUX) 1 else desc2(i)
            }
          }

          newPopulation += desc1
          newPopulation += desc2
        }
      }

      val newEvaluations: Array[Double] = new Array[Double](newPopulation.length)
      newPopulation.zipWithIndex.par.foreach { (chromosome: (Array[Int], Int)) =>
        newEvaluations(chromosome._2) = fitnessFunction(chromosome._1)
        actualEvaluations += 1
      }

      // We order the population. The best ones (greater evaluation value) are the first
      val populationOrder: Array[(Double, Int, String)] = evaluations.zipWithIndex.sortBy((_: (Double, Int))._1)(Ordering[Double].reverse).map((e: (Double, Int)) => (e._1, e._2, "OLD"))
      val newPopulationOrder: Array[(Double, Int, String)] = newEvaluations.zipWithIndex.sortBy((_: (Double, Int))._1)(Ordering[Double].reverse).map((e: (Double, Int)) => (e._1, e._2, "NEW"))

      if (newPopulationOrder.length == 0 || populationOrder.last._1 > newPopulationOrder.head._1) {
        incestThreshold -= 1
      } else {
        val finalOrder: Array[(Double, Int, String)] = (populationOrder ++ newPopulationOrder).sortBy((_: (Double, Int, String))._1)(Ordering[Double].reverse).take(populationSize)

        finalOrder.zipWithIndex.foreach { e: ((Double, Int, String), Int) =>
          population(e._2) = if (e._1._3 == "OLD") population(e._1._2) else newPopulation(e._1._2)
          evaluations(e._2) = if (e._1._3 == "OLD") evaluations(e._1._2) else newEvaluations(e._1._2)
        }
      }

      if (incestThreshold <= 0) {
        population.indices.tail.foreach { i: Int =>
          population(i) = population(i).map((_: Int) => if (random.nextFloat < recombination)
            if (random.nextFloat < prob0to1) 1 else 0 else population(0)(i))
        }

        population.zipWithIndex.tail.par.foreach { (e: (Array[Int], Int)) =>
          evaluations(e._2) = fitnessFunction(e._1)
          actualEvaluations += 1
        }

        incestThreshold = (recombination * (1.0 - recombination) * targetInstances.length.toFloat).toInt
      }
    }

    val bestChromosome: Array[Int] = population(evaluations.zipWithIndex.sortBy((_: (Double, Int))._1)(Ordering[Double].reverse).head._2)
    val finalIndex: Array[Int] = zeroOneToIndex(bestChromosome) map targetInstances
    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = (finalIndex map this.index).sorted map this.data._originalData
    this.data._resultClasses = (finalIndex map this.index).sorted map this.data._originalClasses
    this.data._index = (finalIndex map this.index).sorted

    if (file.isDefined) {
      // Recount of classes
      val newCounter: Map[Any, Int] = (finalIndex map classesToWorkWith).groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(finalIndex.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (finalIndex.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the distance calculation time
      this.logger.addMsg("DISTANCES CALCULATION TIME: %s".format(nanoTimeToString(distancesTime)))
      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_EUS")
    }

    this.data
  }
}

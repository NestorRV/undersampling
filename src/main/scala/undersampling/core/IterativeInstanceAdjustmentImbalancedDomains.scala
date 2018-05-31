package undersampling.core

import java.io.ByteArrayInputStream
import java.util

import com.paypal.digraph.parser.{GraphEdge, GraphNode, GraphParser}
import undersampling.data.Data
import undersampling.util.Utilities._
import weka.classifiers.trees.J48
import weka.core.{Instance, Instances}

import scala.annotation.switch
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Iterative Instance Adjustment for Imbalanced Domains. Original paper: "Addressing imbalanced classification with instance
  * generation techniques: IPADE-ID" by Victoria López, Isaac Triguero, Cristóbal J. Carmona, Salvador García and Francisco Herrera.
  *
  * @param data          localTrainData to work with
  * @param seed          seed to use. If it is not provided, it will use the system time
  * @param minorityClass indicates the minority class. If it's set to -1, it will set to the one with less instances
  * @author Néstor Rodríguez Vico
  */
class IterativeInstanceAdjustmentImbalancedDomains(private[undersampling] val data: Data,
                                                   override private[undersampling] val seed: Long = System.currentTimeMillis(),
                                                   override private[undersampling] val minorityClass: Any = -1) extends Algorithm {

  private[undersampling] val random: scala.util.Random = new scala.util.Random(this.seed)

  /** Compute Iterative Instance Adjustment for Imbalanced Domains undersampling
    *
    * @param file         file to store the log. If its set to None, log process would not be done
    * @param iterations   number of iterations used in Differential Evolution
    * @param strategy     strategy used in the mutation process of Differential Evolution
    * @param randomChoice whether to choose a random individual or not
    * @return Data structure with all the important information
    */
  def sample(file: Option[String] = None, iterations: Int = 100, strategy: Int = 1, randomChoice: Boolean = true): Data = {
    def accuracy(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]], testClasses: Array[Any]): Double = {
      val trainInstances: Instances = buildInstances(data = trainData, classes = trainClasses, fileInfo = this.data._fileInfo)
      val testInstances: Instances = buildInstances(data = testData, classes = testClasses, fileInfo = this.data._fileInfo)

      val j48 = new J48
      j48.setOptions(Array("-U"))
      j48.buildClassifier(trainInstances)

      val realClasses: Array[Double] = (0 until testInstances.numInstances()).map((i: Int) => testInstances.get(i).classValue()).toArray
      val calculatedLabels: Array[Double] = (0 until testInstances.numInstances()).map((i: Int) => j48.classifyInstance(testInstances.get(i))).toArray

      val wellClassified: Int = (realClasses zip calculatedLabels).count((e: (Any, Any)) => e._1 == e._2)
      100.0 * (wellClassified.toFloat / testData.length)
    }

    def computeFitness(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]],
                       testClasses: Array[Any], dict: Map[Any, Double]): Double = {

      val trainInstances: Instances = buildInstances(data = trainData, classes = trainClasses, fileInfo = this.data._fileInfo)
      val testInstances: Instances = buildInstances(data = testData, classes = testClasses, fileInfo = this.data._fileInfo)

      val j48 = new J48
      j48.setOptions(Array("-U"))
      j48.buildClassifier(trainInstances)

      val realClasses: Array[Any] = (0 until testInstances.numInstances()).map((i: Int) => testInstances.get(i).classValue()).toArray
      val calculatedLabels: Array[Any] = (0 until testInstances.numInstances()).map((i: Int) => j48.classifyInstance(testInstances.get(i))).toArray

      val matrix: (Int, Int, Int, Int) = confusionMatrix(originalLabels = realClasses,
        predictedLabels = calculatedLabels, minorityClass = dict(this.untouchableClass))

      val tp: Int = matrix._1
      val fp: Int = matrix._2
      val fn: Int = matrix._3
      val tn: Int = matrix._4

      val tpr: Double = tp / ((tp + fn) + 0.00000001)
      val fpr: Double = fp / ((fp + tn) + 0.00000001)

      val auc: Double = (1.0 + tpr - fpr) / 2.0
      auc
    }

    def selectInitInstances(data: Array[Array[Double]], classes: Array[Any]): (Array[Array[Double]], Array[Any]) = {
      def getCentroid(cluster: Array[Int], data: Array[Array[Double]]): Int = {
        val elements: Array[Array[Double]] = cluster map data
        val centroid: Array[Double] = elements.transpose.map((_: Array[Double]).sum).map((_: Double) / cluster.length)
        (elements.map((instance: Array[Double]) => euclideanDistance(instance, centroid)) zip cluster).minBy((_: (Double, Int))._1)._2
      }

      def getLeafs(instances: Instances, tree: String): Array[String] = {
        val parser = new GraphParser(new ByteArrayInputStream(tree.getBytes()))
        val nodes: util.Map[String, GraphNode] = parser.getNodes
        val edges: util.Map[String, GraphEdge] = parser.getEdges

        (0 until instances.numInstances()).map { i: Int =>
          if(nodes.size() == 1){
            "N0"
          } else {
            val instance: Instance = instances.get(i)
            var returned: Boolean = false
            var currentNode: GraphNode = nodes("N0")
            var leafID: String = ""

            while (!returned) {
              val paths: Array[GraphEdge] = edges.values().filter((p: GraphEdge) => p.getId.startsWith(currentNode.getId)).toArray
              val selectedAttribute: Int = currentNode.getAttribute("label").toString.replaceAll("[^0-9]", "").toInt + 1
              val options: Iterable[Array[String]] = paths.map((p: GraphEdge) => p.getAttribute("label").toString.split(" "))

              val selected: Array[Int] = boolToIndex(options.map { option: Array[String] =>
                option(0) match {
                  case "<=" => (math.floor(instance.value(selectedAttribute) * 1000000) / 1000000) <= option(1).toDouble
                  case ">" => (math.floor(instance.value(selectedAttribute) * 1000000) / 1000000) > option(1).toDouble
                }
              }.toArray)

              currentNode = nodes(paths(selected(0)).getNode2.getId)
              if (currentNode.getAttributes.size() > 2) {
                returned = true
                leafID = currentNode.getId
              }
            }

            leafID
          }

        }.toArray
      }

      val j48 = new J48
      j48.setOptions(Array("-U"))
      val instances: Instances = buildInstances(data = data, classes = classes, fileInfo = this.data._fileInfo)
      j48.buildClassifier(instances)

      val ids: Array[String] = getLeafs(instances = instances, tree = j48.graph())
      val clusters: Map[String, Array[Int]] = ids.zipWithIndex.groupBy((_: (String, Int))._1).mapValues((_: Array[(String, Int)]).unzip._2)

      val selectedElements: Array[Int] = clusters.map { cluster: (String, Array[Int]) =>
        getCentroid(cluster = cluster._2, data = data)
      }.toArray

      val selectedData: Array[Array[Double]] = selectedElements map data
      val selectedClasses: Array[Any] = selectedElements map classes

      val (finalData, finalClasses) = classes.distinct.map { targetClass: Any =>
        if (selectedClasses.indexOf(targetClass) == -1) {
          val targetInstances: Array[Int] = this.random.shuffle(classes.zipWithIndex.collect { case (c, i) if c == targetClass => i }.toList).toArray
          (Array(data(targetInstances(0))) ++ Array(data(targetInstances(1))), Array(classes(targetInstances(0))) ++ Array(classes(targetInstances(1))))
        } else {
          (selectedData, selectedClasses)
        }
      }.unzip

      (finalData.flatten, finalClasses.flatten)
    }

    def differentialEvolution(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]],
                              testClasses: Array[Any], iterations: Int, strategy: Int, dict: Map[Any, Double]): (Array[Array[Double]], Array[Any]) = {

      def mutant(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]], testClasses: Array[Any],
                 fi: Double, strategy: Int = 1): (Array[Array[Double]], Array[Any]) = {

        def getNearestNeighbourWithTheSameClass(element: Array[Double], data: Array[Array[Double]]): Int = {
          val distances: Array[(Double, Int)] = data.map((e: Array[Double]) =>
            euclideanDistance(element, e)).zipWithIndex.sortBy((_: (Double, Int))._1)
          // The first distance is 0, as is computed like the distance between element and element
          distances(1)._2
        }

        val (newPopulation, newClasses): (Array[Array[Double]], Array[Any]) = trainData.indices.map { instance: Int =>
          val sameClassIndex: Array[Int] = testClasses.zipWithIndex.collect { case (c, i) if c == trainClasses(instance) => i }

          val (sameClassData, sameClassLabels): (Array[Array[Double]], Array[Any]) = if (sameClassIndex.length < 5) {
            val auxPopulation: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)
            val auxLabels: ArrayBuffer[Any] = new ArrayBuffer[Any](0)

            val individuals: Array[Array[Double]] = sameClassIndex map trainData
            val labels: Array[Any] = sameClassIndex map trainClasses
            sameClassIndex.indices.foreach { j: Int =>
              val disturbance: Array[Double] = individuals(j).clone

              disturbance.indices.foreach((k: Int) => disturbance(k) = trainData(instance)(k) +
                (-0.01 * j) + ((0.01 * j) - (-0.01 * j)) * this.random.nextDouble)
              auxPopulation += disturbance
              auxLabels += labels(j)
            }

            (individuals ++ auxPopulation, labels ++ auxLabels)
          } else {
            (sameClassIndex map testData, sameClassIndex map testClasses)
          }

          val aux: Array[Int] = sameClassData.indices.toArray
          if (instance < aux.length) aux(instance) = 0
          val randomList: Array[Int] = this.random.shuffle(aux.init.toList).toArray ++ Array(aux.last)

          val r1: Array[Double] = sameClassData(randomList(0))
          val r2: Array[Double] = sameClassData(randomList(1))
          val r3: Array[Double] = sameClassData(randomList(2))
          val r4: Array[Double] = sameClassData(randomList(3))
          val r5: Array[Double] = sameClassData(randomList(4))

          val (newIndividual, newClass): (Array[Double], Any) = (strategy: @switch) match {
            case 1 => ((r1, r2, r3).zipped.toArray.map((e: (Double, Double, Double)) => ((e._2 - e._3) * fi) + e._1), sameClassLabels(randomList(1)))
            case 2 => val prod1: Array[Double] = (r1 zip r2).map((e: (Double, Double)) => (e._1 - e._2) * fi)
              val nearestNeighbor: Array[Double] = trainData(getNearestNeighbourWithTheSameClass(element = trainData(instance), data = testData))
              val prod2: Array[Double] = (nearestNeighbor zip trainData(instance)).map((e: (Double, Double)) => (e._1 - e._2) * fi)
              val prod: Array[Double] = (prod1 zip prod2).map((e: (Double, Double)) => e._1 + e._2)
              ((trainData(instance) zip prod).map((e: (Double, Double)) => e._1 + e._2), trainClasses(instance))
            case 3 => val random: Double = this.random.nextDouble
              val prod1: Array[Double] = (r2 zip r3).map((e: (Double, Double)) => (e._1 - e._2) * fi * random)
              val prod2: Array[Double] = (r1 zip trainData(instance)).map((e: (Double, Double)) => (e._1 - e._2) * fi * random)
              ((prod1 zip prod2).map((e: (Double, Double)) => e._1 + e._2), sameClassLabels(randomList(1)))
            case 4 => val prod1: Array[Double] = (r2 zip r3).map((e: (Double, Double)) => (e._1 - e._2) * fi)
              val prod2: Array[Double] = (r4 zip r5).map((e: (Double, Double)) => (e._1 - e._2) * fi)
              ((prod1 zip prod2).map((e: (Double, Double)) => e._1 + e._2), sameClassLabels(randomList(1)))
            case _ => throw new Exception("Invalid strategy: strategy should be: 1, 2, 3 or 4.")
          }

          (newIndividual, newClass)
        }.toArray.unzip

        (newPopulation.map((instance: Array[Double]) => instance.map((e: Double) => if (e > 1) 1 else if (e < 0) 0 else e)), newClasses)
      }

      def lsff(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]],
               testClasses: Array[Any], fi: Double, strategy: Int): Double = {
        val (newPopulation, newClasses): (Array[Array[Double]], Array[Any]) = mutant(trainData = trainData, trainClasses = trainClasses,
          testData = testData, testClasses = testClasses, fi = fi, strategy = strategy)
        computeFitness(trainData = newPopulation, trainClasses = newClasses, testData = testData, testClasses = testClasses, dict = dict)
      }

      def SFGSS(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]], testClasses: Array[Any],
                iterations: Int = 8, strategy: Int): (Array[Array[Double]], Array[Any]) = {
        var a: Double = 0.1
        var b: Double = 1.0
        var fi1: Double = 0.0
        var fi2: Double = 0.0
        var fitness1: Double = 0.0
        var fitness2: Double = 0.0

        (0 until iterations).foreach { _: Int =>
          fi1 = b - (b - a) / ((1 + Math.sqrt(5)) / 5)
          fi2 = 0.1 + (b - a) / ((1 + Math.sqrt(5)) / 5)

          fitness1 = lsff(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = fi1, strategy = strategy)
          fitness2 = lsff(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = fi2, strategy = strategy)

          if (fitness1 > fitness2) b = fi2 else a = fi1
        }

        val scaling: Double = if (fitness1 > fitness2) fi2 else fi1

        mutant(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = scaling, strategy = strategy)
      }

      def SFHC(trainData: Array[Array[Double]], trainClasses: Array[Any], testData: Array[Array[Double]], testClasses: Array[Any],
               scalingFactor: Double, iterations: Int = 20, strategy: Int): (Array[Array[Double]], Array[Any]) = {
        var h: Double = 0.5
        var fitness1: Double = 0.0
        var fitness2: Double = 0.0
        var fitness3: Double = 0.0
        var bestFitness: Double = 0.0
        var localScalingFactor: Double = scalingFactor

        (0 until iterations).foreach { _: Int =>
          fitness1 = lsff(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = localScalingFactor - h, strategy = strategy)
          fitness2 = lsff(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = localScalingFactor, strategy = strategy)
          fitness3 = lsff(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = localScalingFactor + h, strategy = strategy)

          if (fitness1 >= fitness2 && fitness1 >= fitness3) {
            bestFitness = localScalingFactor - h
          } else if (fitness2 >= fitness1 && fitness2 >= fitness3) {
            bestFitness = localScalingFactor
            h = h / 2
          } else {
            bestFitness = localScalingFactor
          }

          localScalingFactor = bestFitness
        }

        mutant(trainData = trainData, trainClasses = trainClasses, testData = testData, testClasses = testClasses, fi = scalingFactor, strategy = strategy)
      }

      var localTrainData: Array[Array[Double]] = trainData.clone
      var localTrainClasses: Array[Any] = trainClasses.clone
      val randJ: Double = this.random.nextDouble()
      val tau: Array[Double] = Array(this.random.nextDouble(), this.random.nextDouble())

      var fitness: Double = computeFitness(trainData = localTrainData, trainClasses = localTrainClasses,
        testData = testData, testClasses = testClasses, dict = dict)

      (0 until iterations).foreach { iteration: Int =>
        val (newPopulation, newClasses): (Array[Array[Double]], Array[Any]) = if (iteration % 10 == 0) {
          if (randJ < tau(0)) {
            SFGSS(trainData = localTrainData, trainClasses = localTrainClasses, testData = testData,
              testClasses = testClasses, strategy = strategy)
          } else if (tau(0) <= randJ && randJ < tau(1)) {
            SFHC(trainData = localTrainData, trainClasses = localTrainClasses, testData = testData,
              testClasses = testClasses, scalingFactor = 0.5, strategy = strategy)
          } else {
            (localTrainData.clone, localTrainClasses.clone)
          }
        } else {
          val scalingFactor: Double = this.random.nextDouble
          localTrainData.indices.map { instance: Int =>
            val sameClassIndex: Array[Int] = testClasses.zipWithIndex.collect { case (c, i) if c == localTrainClasses(instance) => i }

            val (sameClassData, sameClassLabels): (Array[Array[Double]], Array[Any]) = if (sameClassIndex.length < 3) {
              val auxPopulation: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]](0)
              val auxLabels: ArrayBuffer[Any] = new ArrayBuffer[Any](0)

              val individuals: Array[Array[Double]] = sameClassIndex map localTrainData
              val labels: Array[Any] = sameClassIndex map localTrainClasses
              sameClassIndex.indices.foreach { j: Int =>
                val disturbance: Array[Double] = individuals(j).clone

                disturbance.indices.foreach((k: Int) => disturbance(k) = localTrainData(instance)(k) +
                  (-0.01 * j) + ((0.01 * j) - (-0.01 * j)) * this.random.nextDouble)
                auxPopulation += disturbance
                auxLabels += labels(j)
              }

              (individuals ++ auxPopulation, labels ++ auxLabels)
            } else {
              (sameClassIndex map testData, sameClassIndex map testClasses)
            }

            val randomList: Array[Int] = this.random.shuffle(sameClassData.indices.toList).toArray

            val r1: Array[Double] = sameClassData(randomList(0))
            val r2: Array[Double] = sameClassData(randomList(1))
            val r3: Array[Double] = sameClassData(randomList(2))

            val random: Double = this.random.nextDouble
            val prod1: Array[Double] = (r2 zip r3).map((e: (Double, Double)) => (e._1 - e._2) * scalingFactor * random)
            val prod2: Array[Double] = (r1 zip localTrainData(instance)).map((e: (Double, Double)) => (e._1 - e._2) * random)
            val crossOver: Array[Double] = (prod1, prod2, localTrainData(instance)).zipped.toArray.map((e: (Double, Double, Double)) => e._1 + e._2 + e._3)

            (crossOver.map((e: Double) => if (e > 1) 1 else if (e < 0) 0 else e), sameClassLabels(randomList(1)))
          }.toArray.unzip
        }

        val trialFitness: Double = computeFitness(trainData = newPopulation, trainClasses = newClasses,
          testData = testData, testClasses = testClasses, dict = dict)
        if (trialFitness > fitness) {
          fitness = trialFitness
          localTrainData = newPopulation
          localTrainClasses = newClasses
        }
      }

      (localTrainData, localTrainClasses)
    }

    // Use normalized localTrainData and randomized localTrainData
    val dataToWorkWith: Array[Array[Double]] = (this.index map zeroOneNormalization(this.data)).toArray

    // and randomized localTrainClasses to match the randomized localTrainData
    val classesToWorkWith: Array[Any] = (this.index map this.y).toArray

    // Start the time
    val initTime: Long = System.nanoTime()

    var counter: Double = -1.0
    val classesTranslation: Map[Any, Double] = classesToWorkWith.distinct.map { value: Any => counter += 1.0; value -> counter }.toMap

    val initInstances: (Array[Array[Double]], Array[Any]) = selectInitInstances(data = dataToWorkWith, classes = classesToWorkWith)
    var (population, classes): (Array[Array[Double]], Array[Any]) = differentialEvolution(trainData = initInstances._1,
      trainClasses = initInstances._2, testData = dataToWorkWith, testClasses = classesToWorkWith,
      iterations = iterations, strategy = strategy, dict = classesTranslation)

    var fitness: Double = computeFitness(trainData = population, trainClasses = classes, testData = dataToWorkWith,
      testClasses = classesToWorkWith, dict = classesTranslation)

    val isClassMarked: mutable.Map[Any, Boolean] = mutable.Map[Any, Boolean]()
    classesToWorkWith.distinct.foreach((c: Any) => isClassMarked(c) = false)

    val contOptimizedPositive: mutable.Map[Any, Int] = mutable.Map[Any, Int]()
    classesToWorkWith.distinct.foreach((c: Any) => contOptimizedPositive(c) = 0)

    val optimizedIteration: mutable.Map[Any, Int] = mutable.Map[Any, Int]()
    classesToWorkWith.distinct.foreach((c: Any) => optimizedIteration(c) = 1)

    val fitnessClass: mutable.Map[Any, Double] = mutable.Map[Any, Double]()
    classesToWorkWith.distinct.foreach((c: Any) => fitnessClass(c) = 0.0)

    var alternativeData: Array[Array[Double]] = new Array[Array[Double]](0)
    var alternativeClasses: Array[Any] = new Array[Any](0)
    while (!isClassMarked.forall((e: (Any, Boolean)) => e._2)) {
      var actualFitness: Double = Double.MaxValue
      var targetClass: Any = -1

      classesToWorkWith.distinct.zipWithIndex.foreach { j: (Any, Int) =>
        val sameClassIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (c, i) if c == j._1 => i }
        if (sameClassIndex.length > 1) {
          fitnessClass(j._1) = accuracy(trainData = population, trainClasses = classes,
            testData = sameClassIndex map dataToWorkWith, testClasses = sameClassIndex map classesToWorkWith)

          if (fitnessClass(j._1) < actualFitness && !isClassMarked(j._1)) {
            actualFitness = fitnessClass(j._1)
            targetClass = j._1
          }
        } else {
          isClassMarked(j._1) = true
        }
      }

      if (!isClassMarked(targetClass)) {
        val (population2Data, population2Classes): (Array[Array[Double]], Array[Any]) = if (targetClass == this.untouchableClass
          && contOptimizedPositive(targetClass) > 0) {
          (alternativeData.clone, alternativeClasses.clone)
        } else {
          val sameClassIndex: Array[Int] = classesToWorkWith.zipWithIndex.collect { case (c, i) if c == targetClass => i }
          val (newIndividual, newClass): (Array[Double], Any) = if (randomChoice || targetClass != this.untouchableClass) {
            val randomElement: Int = this.random.shuffle(sameClassIndex.toList).head
            (dataToWorkWith(randomElement), classesToWorkWith(randomElement))
          } else {
            var farthest: Int = 0
            var farthestDistance: Double = Double.MaxValue
            val sameClassData: Array[Array[Double]] = sameClassIndex map dataToWorkWith
            val sameClassLabels: Array[Any] = sameClassIndex map classesToWorkWith
            sameClassIndex.foreach { z: Int =>
              val distances: Double = population.indices.map { h: Int =>
                val aux: Double = (sameClassData(z) zip population(h)).map((e: (Double, Double)) => Math.abs(e._1 - e._2)).sum
                if (aux != 0.0) aux else Double.MaxValue
              }.sum

              if (distances < farthestDistance && distances != 0) {
                farthestDistance = distances
                farthest = z
              }
            }

            (sameClassData(farthest), sameClassLabels(farthest))
          }

          (population.clone ++ Array(newIndividual), classes.clone ++ Array(newClass))
        }

        val (testerData, testerClasses): (Array[Array[Double]], Array[Any]) = differentialEvolution(trainData = population2Data,
          trainClasses = population2Classes, testData = dataToWorkWith, testClasses = classesToWorkWith,
          iterations = iterations, strategy = strategy, dict = classesTranslation)

        fitness = computeFitness(trainData = population, trainClasses = classes, testData = dataToWorkWith,
          testClasses = classesToWorkWith, dict = classesTranslation)
        val trialFitness: Double = computeFitness(trainData = testerData, trainClasses = testerClasses,
          testData = dataToWorkWith, testClasses = classesToWorkWith, dict = classesTranslation)

        if (trialFitness > fitness) {
          optimizedIteration(targetClass) += 1
          population = testerData.clone
          classes = testerClasses.clone
          contOptimizedPositive(targetClass) = 0
        } else if (targetClass == this.untouchableClass && optimizedIteration(targetClass) < 10) {
          optimizedIteration(targetClass) += 1
          population = testerData.clone
          classes = testerClasses.clone
        } else {
          if (targetClass == this.untouchableClass) {
            alternativeData = testerData.clone
            alternativeClasses = testerClasses.clone

            contOptimizedPositive(targetClass) += 1

            if (contOptimizedPositive(targetClass) >= 10) {
              isClassMarked(targetClass) = true
            }
          } else {
            isClassMarked(targetClass) = true
          }
        }
      }
    }

    // Stop the time
    val finishTime: Long = System.nanoTime()

    this.data._resultData = population.map((row: Array[Double]) => row.map((e: Double) => e.asInstanceOf[Any]))
    this.data._resultClasses = classes
    this.data._index = null

    if (file.isDefined) {
      // Recount of localTrainClasses
      val newCounter: Map[Any, Int] = classes.groupBy(identity).mapValues((_: Array[Any]).length)

      this.logger.addMsg("ORIGINAL SIZE: %d".format(dataToWorkWith.length))
      this.logger.addMsg("NEW DATA SIZE: %d".format(classes.length))
      this.logger.addMsg("REDUCTION PERCENTAGE: %s".format(100 - (classes.length.toFloat / dataToWorkWith.length) * 100))

      this.logger.addMsg("ORIGINAL IMBALANCED RATIO: %s".format(imbalancedRatio(this.counter, this.untouchableClass)))
      // Recompute the Imbalanced Ratio
      this.logger.addMsg("IMBALANCED RATIO: %s".format(imbalancedRatio(newCounter, this.untouchableClass)))

      // Save the time
      this.logger.addMsg("TOTAL ELAPSED TIME: %s".format(nanoTimeToString(finishTime - initTime)))

      // Save the log
      this.logger.storeFile(file.get + "_IPADE-ID")
    }

    this.data
  }
}

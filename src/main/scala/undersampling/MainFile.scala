package undersampling

import java.io.{File, PrintWriter}

import undersampling.core._
import undersampling.data.Data
import undersampling.io.{Reader, Writer}
import undersampling.util._
import weka.classifiers.Evaluation
import weka.classifiers.functions.SMO
import weka.classifiers.trees.J48
import weka.core.Instances

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.sqrt

/** An object to test the different algorithms
  *
  * @author Néstor Rodríguez Vico
  */
object MainFile {

  def applyAlgorithms(): Unit = {
    val writer = new Writer
    val reader = new Reader
    val datasets: List[String] = new File("./input/dat").listFiles.filter((_: File).isFile).toList.map((_: File).getName()).sorted

    datasets.zipWithIndex.foreach { dataset: (String, Int) =>
      val datasetName: String = dataset._1.replaceAll(".dat", "")
      val data: Data = reader.readArff(file = "./input/dat/" + dataset._1)

      var result: Data = null
      println("%s de %s -> %s".format(dataset._2, datasets.length - 1, datasetName))

      println("1 de 15: BalanceCascade")
      val bc = new BalanceCascade(data, seed = 0L)
      result = bc.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_BC.dat", data = result)

      println("2 de 15: ClassPurityMaximization")
      val cpm = new ClassPurityMaximization(data, seed = 0L)
      result = cpm.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_CPM.dat", data = result)

      println("3 de 15: ClusterOSS")
      val cOSS = new ClusterOSS(data, seed = 0L)
      result = cOSS.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_ClusterOSS.dat", data = result)

      println("4 de 15: CondensedNearestNeighbor")
      val cnn = new CondensedNearestNeighbor(data, seed = 0L)
      result = cnn.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_CNN.dat", data = result)

      println("5 de 15: EasyEnsemble")
      val ee = new EasyEnsemble(data, seed = 0L)
      result = ee.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_EE.dat", data = result)

      println("6 de 15: EditedNearestNeighbor")
      val enn = new EditedNearestNeighbor(data, seed = 0L)
      result = enn.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_ENN.dat", data = result)

      println("7 de 15: EvolutionaryUnderSampling")
      val eus = new EvolutionaryUnderSampling(data, seed = 0L)
      result = eus.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_EUS.dat", data = result)

      println("8 de 15: InstanceHardnessThreshold")
      val ihts = new InstanceHardnessThreshold(data, seed = 0L)
      result = ihts.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_IHTS.dat", data = result)

      println("9 de 15: IterativeInstanceAdjustmentImbalancedDomains")
      val ipade = new IterativeInstanceAdjustmentImbalancedDomains(data, seed = 0L)
      result = ipade.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_IPADE.dat", data = result)

      println("10 de 15: NearMiss")
      val nm = new NearMiss(data, seed = 0L)
      result = nm.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_NM.dat", data = result)

      println("11 de 15: NeighbourhoodCleaningRule")
      val ncl = new NeighbourhoodCleaningRule(data, seed = 0L)
      result = ncl.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_NCL.dat", data = result)

      println("12 de 15: OneSideSelection")
      val oss = new OneSideSelection(data, seed = 0L)
      result = oss.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_OSS.dat", data = result)

      println("13 de 15: RandomUndersampling")
      val ru = new RandomUndersampling(data, seed = 0L)
      result = ru.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_RU.dat", data = result)

      println("14 de 15: TomekLink")
      val tl = new TomekLink(data, seed = 0L)
      result = tl.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_TL.dat", data = result)

      println("15 de 15: UndersamplingBasedClustering")
      val sbc = new UndersamplingBasedClustering(data, seed = 0L)
      result = sbc.sample(file = Option("./input/logs/" + datasetName))
      writer.writeArff(file = "./input/results/" + datasetName + "_SBC.dat", data = result)

      println("*******************************************************")
    }
  }

  def measureAlgorithms(): Unit = {
    val writerFolds = new PrintWriter(new File("input/reports/processed.csv"))
    val writerMeans = new PrintWriter(new File("input/reports/processed_means.csv"))
    writerFolds.write("Dataset,Algoritmo,j48auc,j48gm,j48_%,SVMauc,SVMgm,SVM_%\n")
    writerMeans.write("Dataset,Algoritmo,j48auc,j48gm,j48_%,SVMauc,SVMgm,SVM_%\n")

    val algorithms: mutable.Map[String, Data => Data] = mutable.Map("BC" -> { (d: Data) => new BalanceCascade(d, seed = 0L).sample() })
    algorithms += ("CPM" -> { (d: Data) => new ClassPurityMaximization(d, seed = 0L).sample() })
    algorithms += ("ClusterOSS" -> { (d: Data) => new ClusterOSS(d, seed = 0L).sample() })
    algorithms += ("CNN" -> { (d: Data) => new CondensedNearestNeighbor(d, seed = 0L).sample() })
    algorithms += ("EE" -> { (d: Data) => new EasyEnsemble(d, seed = 0L).sample() })
    algorithms += ("ENN" -> { (d: Data) => new EditedNearestNeighbor(d, seed = 0L).sample() })
    algorithms += ("EUS" -> { (d: Data) => new EvolutionaryUnderSampling(d, seed = 0L).sample() })
    algorithms += ("IHTS" -> { (d: Data) => new InstanceHardnessThreshold(d, seed = 0L).sample() })
    algorithms += ("IPADE-ID" -> { (d: Data) => new IterativeInstanceAdjustmentImbalancedDomains(d, seed = 0L).sample() })
    algorithms += ("NM" -> { (d: Data) => new NearMiss(d, seed = 0L).sample() })
    algorithms += ("NCL" -> { (d: Data) => new NeighbourhoodCleaningRule(d, seed = 0L).sample() })
    algorithms += ("OSS" -> { (d: Data) => new OneSideSelection(d, seed = 0L).sample() })
    algorithms += ("RU" -> { (d: Data) => new RandomUndersampling(d, seed = 0L).sample() })
    algorithms += ("TL" -> { (d: Data) => new TomekLink(d, seed = 0L).sample() })
    algorithms += ("SCB" -> { (d: Data) => new UndersamplingBasedClustering(d, seed = 0L).sample() })

    val reader = new Reader
    val datasets: List[String] = new File("./input/dat/").listFiles.filter((_: File).isFile).toList.map((_: File).getName()).sorted

    datasets.zipWithIndex.foreach { dataset: (String, Int) =>
      val datasetName: String = dataset._1.replaceAll(".dat", "")
      val data: Data = reader.readArff(file = "./input/dat/" + dataset._1)
      val random: scala.util.Random = new scala.util.Random(0)

      algorithms.toSeq.sortBy((_: (String, Data => Data))._1).foreach { alg: (String, Data => Data) =>

        val x: Array[Array[Double]] = Utilities.processData(data)
        val y: Array[Any] = data.originalClasses
        val minClass: Any = y.groupBy(identity).mapValues((_: Array[Any]).length).minBy((c: (Any, Int)) => c._2)._1

        val minElements: Array[Int] = y.zipWithIndex.collect { case (c, i) if c == minClass => i }
        val majElements: Array[Int] = y.zipWithIndex.collect { case (c, i) if c != minClass => i }

        val minFolds: List[List[Int]] = random.shuffle(minElements.toList).grouped((minElements.length.toFloat / 5).ceil.toInt).toList
        val majFolds: List[List[Int]] = random.shuffle(majElements.toList).grouped((majElements.length.toFloat / 5).ceil.toInt).toList

        val folds: List[List[Int]] = (minFolds zip majFolds).map((pair: (List[Int], List[Int])) => pair._1 ++ pair._2)

        val aucJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
        val gmJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
        val perJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
        val aucSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
        val gmSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
        val perSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)

        folds.zipWithIndex.foreach { test: (List[Int], Int) =>

          val trainData: Data = new Data(_nominal = data._nominal, _originalData = (y.indices.diff(test._1) map data._originalData).toArray,
            _originalClasses = (y.indices.diff(test._1) map y).toArray, _fileInfo = data._fileInfo)

          val result: Data = alg._2(trainData)

          val testX: Array[Array[Double]] = (test._1 map x).toArray
          val testY: Array[Any] = (test._1 map y).toArray

          val trainInstances: Instances = if (alg._1 != "IPADE-ID") {
            val selectedIndex: Array[Int] = result._index map y.indices.diff(test._1)
            Utilities.buildInstances(selectedIndex map x, selectedIndex map y, data._fileInfo)
          } else {
            val ipadeX: Array[Array[Double]] = result._resultData.map((row: Array[Any]) => row.map((e: Any) => e.asInstanceOf[Double]))
            Utilities.buildInstances(ipadeX, result._resultClasses, data._fileInfo)
          }

          val testInstances: Instances = Utilities.buildInstances(testX, testY, data._fileInfo)

          val j48: J48 = new J48
          j48.setOptions(Array("-U"))
          j48.buildClassifier(trainInstances)

          val svm: SMO = new SMO
          svm.setOptions(Array("-C", "1.0", "-L", "0.001"))
          svm.buildClassifier(trainInstances)

          val evaluationsJ48: Evaluation = new Evaluation(trainInstances)
          evaluationsJ48.evaluateModel(j48, testInstances)
          val percentageJ48: Double = evaluationsJ48.pctCorrect()
          val tprJ48: Double = evaluationsJ48.truePositiveRate(testInstances.classIndex())
          val tnrJ48: Double = evaluationsJ48.trueNegativeRate(testInstances.classIndex())
          val aucJ48: Double = evaluationsJ48.areaUnderROC(testInstances.classIndex())
          val gmJ48: Double = sqrt(tprJ48 * tnrJ48)

          val evaluationsSVM: Evaluation = new Evaluation(trainInstances)
          evaluationsSVM.evaluateModel(svm, testInstances)
          val percentageSVM: Double = evaluationsSVM.pctCorrect()
          val tprSVM: Double = evaluationsSVM.truePositiveRate(testInstances.classIndex())
          val tnrSVM: Double = evaluationsSVM.trueNegativeRate(testInstances.classIndex())
          val aucSVM: Double = evaluationsSVM.areaUnderROC(testInstances.classIndex())
          val gmSVM: Double = sqrt(tprSVM * tnrSVM)

          aucJ48Array += aucJ48
          gmJ48Array += gmJ48
          perJ48Array += percentageJ48
          aucSVMArray += aucSVM
          gmSVMArray += gmSVM
          perSVMArray += percentageSVM

          writerFolds.write("%s_fold_%s,%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, test._2, alg._1, "%1.3f".format(aucJ48), "%1.3f".format(gmJ48),
            "%1.3f".format(percentageJ48), "%1.3f".format(aucSVM), "%1.3f".format(gmSVM), "%1.3f".format(percentageSVM)))

          print("%s_fold_%s,%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, test._2, alg._1, "%1.3f".format(aucJ48), "%1.3f".format(gmJ48),
            "%1.3f".format(percentageJ48), "%1.3f".format(aucSVM), "%1.3f".format(gmSVM), "%1.3f".format(percentageSVM)))
        }

        val aucJ48Mean: Double = aucJ48Array.sum / aucJ48Array.length
        val gmJ48Mean: Double = gmJ48Array.sum / gmJ48Array.length
        val perJ48Mean: Double = perJ48Array.sum / perJ48Array.length
        val aucSVMMean: Double = aucSVMArray.sum / aucSVMArray.length
        val gmSVMMean: Double = gmSVMArray.sum / gmSVMArray.length
        val perSVMMean: Double = perSVMArray.sum / perSVMArray.length

        writerMeans.write("%s,%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, alg._1, "%1.3f".format(aucJ48Mean), "%1.3f".format(gmJ48Mean),
          "%1.3f".format(perJ48Mean), "%1.3f".format(aucSVMMean), "%1.3f".format(gmSVMMean), "%1.3f".format(perSVMMean)))

        print("%s,%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, alg._1, "%1.3f".format(aucJ48Mean), "%1.3f".format(gmJ48Mean),
          "%1.3f".format(perJ48Mean), "%1.3f".format(aucSVMMean), "%1.3f".format(gmSVMMean), "%1.3f".format(perSVMMean)))

        aucJ48Array.clear()
        gmJ48Array.clear()
        perJ48Array.clear()
        aucSVMArray.clear()
        gmSVMArray.clear()
        perSVMArray.clear()
      }
    }

    writerFolds.close()
    writerMeans.close()
  }

  def measureRealData(): Unit = {
    val writerFolds = new PrintWriter(new File("input/reports/original.csv"))
    val writerMeans = new PrintWriter(new File("input/reports/original_means.csv"))
    writerFolds.write("name,j48auc,j48gm,j48_%,SVMauc,SVMgm,SVM_%\n")
    writerMeans.write("name,j48auc,j48gm,j48_%,SVMauc,SVMgm,SVM_%\n")

    val reader = new Reader
    val datasets: List[String] = new File("./input/dat/").listFiles.filter((_: File).isFile).toList.map((_: File).getName()).sorted

    datasets.zipWithIndex.foreach { dataset: (String, Int) =>
      val datasetName: String = dataset._1.replaceAll(".dat", "")
      val data: Data = reader.readArff(file = "./input/dat/" + dataset._1)
      val random: scala.util.Random = new scala.util.Random(0)

      val x: Array[Array[Double]] = Utilities.processData(data)
      val y: Array[Any] = data.originalClasses
      val minClass: Any = y.groupBy(identity).mapValues((_: Array[Any]).length).minBy((c: (Any, Int)) => c._2)._1

      val minElements: Array[Int] = y.zipWithIndex.collect { case (c, i) if c == minClass => i }
      val majElements: Array[Int] = y.zipWithIndex.collect { case (c, i) if c != minClass => i }

      val minFolds: List[List[Int]] = random.shuffle(minElements.toList).grouped((minElements.length.toFloat / 5).ceil.toInt).toList
      val majFolds: List[List[Int]] = random.shuffle(majElements.toList).grouped((majElements.length.toFloat / 5).ceil.toInt).toList

      val folds: List[List[Int]] = (minFolds zip majFolds).map((pair: (List[Int], List[Int])) => pair._1 ++ pair._2)

      val aucJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
      val gmJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
      val perJ48Array: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
      val aucSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
      val gmSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)
      val perSVMArray: ArrayBuffer[Double] = new ArrayBuffer[Double](0)

      folds.zipWithIndex.foreach { test: (List[Int], Int) =>
        val trainData: Array[Array[Double]] = (y.indices.diff(test._1) map x).toArray
        val testData: Array[Array[Double]] = (test._1 map x).toArray
        val trainClasses: Array[Any] = (y.indices.diff(test._1) map y).toArray
        val testClasses: Array[Any] = (test._1 map y).toArray

        val trainInstances: Instances = Utilities.buildInstances(trainData, trainClasses, data._fileInfo)
        val testInstances: Instances = Utilities.buildInstances(testData, testClasses, data._fileInfo)

        val j48 = new J48
        j48.setOptions(Array("-U"))
        j48.buildClassifier(trainInstances)

        val svm: SMO = new SMO
        svm.setOptions(Array("-C", "1.0", "-L", "0.001"))
        svm.buildClassifier(trainInstances)

        val evaluationsJ48: Evaluation = new Evaluation(trainInstances)
        evaluationsJ48.evaluateModel(j48, testInstances)
        val percentageJ48: Double = evaluationsJ48.pctCorrect()
        val tprJ48: Double = evaluationsJ48.truePositiveRate(testInstances.classIndex())
        val tnrJ48: Double = evaluationsJ48.trueNegativeRate(testInstances.classIndex())
        val aucJ48: Double = evaluationsJ48.areaUnderROC(testInstances.classIndex())
        val gmJ48: Double = sqrt(tprJ48 * tnrJ48)

        val evaluationsSVM: Evaluation = new Evaluation(trainInstances)
        evaluationsSVM.evaluateModel(svm, testInstances)
        val percentageSVM: Double = evaluationsSVM.pctCorrect()
        val tprSVM: Double = evaluationsSVM.truePositiveRate(testInstances.classIndex())
        val tnrSVM: Double = evaluationsSVM.trueNegativeRate(testInstances.classIndex())
        val aucSVM: Double = evaluationsSVM.areaUnderROC(testInstances.classIndex())
        val gmSVM: Double = sqrt(tprSVM * tnrSVM)

        aucJ48Array += aucJ48
        gmJ48Array += gmJ48
        perJ48Array += percentageJ48
        aucSVMArray += aucSVM
        gmSVMArray += gmSVM
        perSVMArray += percentageSVM

        writerFolds.write("%s_fold_%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, test._2, "%1.3f".format(aucJ48), "%1.3f".format(gmJ48), "%1.3f".format(percentageJ48),
          "%1.3f".format(aucSVM), "%1.3f".format(gmSVM), "%1.3f".format(percentageSVM)))

        print("%s_fold_%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, test._2, "%1.3f".format(aucJ48), "%1.3f".format(gmJ48), "%1.3f".format(percentageJ48),
          "%1.3f".format(aucSVM), "%1.3f".format(gmSVM), "%1.3f".format(percentageSVM)))
      }

      val aucJ48Mean: Double = aucJ48Array.sum / aucJ48Array.length
      val gmJ48Mean: Double = gmJ48Array.sum / gmJ48Array.length
      val perJ48Mean: Double = perJ48Array.sum / perJ48Array.length
      val aucSVMMean: Double = aucSVMArray.sum / aucSVMArray.length
      val gmSVMMean: Double = gmSVMArray.sum / gmSVMArray.length
      val perSVMMean: Double = perSVMArray.sum / perSVMArray.length

      writerMeans.write("%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, "%1.3f".format(aucJ48Mean), "%1.3f".format(gmJ48Mean), "%1.3f".format(perJ48Mean),
        "%1.3f".format(aucSVMMean), "%1.3f".format(gmSVMMean), "%1.3f".format(perSVMMean)))

      print("%s,%s,%s,%s,%s,%s,%s\n".format(datasetName, "%1.3f".format(aucJ48Mean), "%1.3f".format(gmJ48Mean), "%1.3f".format(perJ48Mean),
        "%1.3f".format(aucSVMMean), "%1.3f".format(gmSVMMean), "%1.3f".format(perSVMMean)))

      aucJ48Array.clear()
      gmJ48Array.clear()
      perJ48Array.clear()
      aucSVMArray.clear()
      gmSVMArray.clear()
      perSVMArray.clear()
    }

    writerFolds.close()
    writerMeans.close()
  }

  def createImbalancedSet(): Unit = {
    val writer = new Writer
    val reader = new Reader
    val data: Data = reader.readArff(file = "./input/graphs/banana.dat")

    val minClass: Any = data.originalClasses.groupBy(identity).mapValues((_: Array[Any]).length).minBy((c: (Any, Int)) => c._2)._1

    val minorityIndex: Array[Int] = data.originalClasses.zipWithIndex.collect { case (label, i) if label == minClass => i }
    val majorityIndex: Array[Int] = data.originalClasses.zipWithIndex.collect { case (label, i) if label != minClass => i }
    val random: scala.util.Random = new scala.util.Random(0)

    data._index = random.shuffle(minorityIndex.toList).take((minorityIndex.length * 1).toInt).toArray ++ majorityIndex
    data._resultClasses = data._index map data.originalClasses
    data._resultData = data._index map data.originalData

    writer.writeArff(file = "./input/graphs/aux_banana_imbalanced.dat", data = data)
    writer.writeDelimitedText(file = "./input/graphs/banana_imbalanced.dat", data = data)
  }

  def computeDatasetsForGraphs(): Unit = {
    val writer = new Writer
    val reader = new Reader
    val data: Data = reader.readArff(file = "./input/graphs/aux_banana_imbalanced.dat")

    var result: Data = null

    println("1 de 15: BalanceCascade")
    val bc = new BalanceCascade(data, seed = 0L)
    result = bc.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_BC.dat", data = result)

    println("2 de 15: ClassPurityMaximization")
    val cpm = new ClassPurityMaximization(data, seed = 0L)
    result = cpm.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_CPM.dat", data = result)

    println("3 de 15: ClusterOSS")
    val cOSS = new ClusterOSS(data, seed = 0L)
    result = cOSS.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_ClusterOSS.dat", data = result)

    println("4 de 15: CondensedNearestNeighbor")
    val cnn = new CondensedNearestNeighbor(data, seed = 0L)
    result = cnn.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_CNN.dat", data = result)

    println("5 de 15: EasyEnsemble")
    val ee = new EasyEnsemble(data, seed = 0L)
    result = ee.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_EE.dat", data = result)

    println("6 de 15: EditedNearestNeighbor")
    val enn = new EditedNearestNeighbor(data, seed = 0L)
    result = enn.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_ENN.dat", data = result)

    println("7 de 15: EvolutionaryUnderSampling")
    val eus = new EvolutionaryUnderSampling(data, seed = 0L)
    result = eus.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_EUS.dat", data = result)

    println("8 de 15: InstanceHardnessThreshold")
    val ihts = new InstanceHardnessThreshold(data, seed = 0L)
    result = ihts.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_IHTS.dat", data = result)

    println("9 de 15: IterativeInstanceAdjustmentImbalancedDomains")
    val ipade = new IterativeInstanceAdjustmentImbalancedDomains(data, seed = 0L)
    result = ipade.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_IPADE.dat", data = result)

    println("10 de 15: NearMiss")
    val nm = new NearMiss(data, seed = 0L)
    result = nm.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_NM.dat", data = result)

    println("11 de 15: NeighbourhoodCleaningRule")
    val ncl = new NeighbourhoodCleaningRule(data, seed = 0L)
    result = ncl.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_NCL.dat", data = result)

    println("12 de 15: OneSideSelection")
    val oss = new OneSideSelection(data, seed = 0L)
    result = oss.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_OSS.dat", data = result)

    println("13 de 15: RandomUndersampling")
    val ru = new RandomUndersampling(data, seed = 0L)
    result = ru.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_RU.dat", data = result)

    println("14 de 15: TomekLink")
    val tl = new TomekLink(data, seed = 0L)
    result = tl.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_TL.dat", data = result)

    println("15 de 15: UndersamplingBasedClustering")
    val sbc = new UndersamplingBasedClustering(data, seed = 0L)
    result = sbc.sample()
    writer.writeDelimitedText(file = "./input/graphs/banana_SBC.dat", data = result)
  }

  def main(args: Array[String]): Unit = {
    //applyAlgorithms()
    measureAlgorithms()
    measureRealData()
    //createImbalancedSet()
    //computeDatasetsForGraphs()
  }
}
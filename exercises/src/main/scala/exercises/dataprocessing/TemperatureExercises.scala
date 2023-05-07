package exercises.dataprocessing

import scala.concurrent.ExecutionContext

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] =
    samples.parFoldMap(sample => Option(sample))(Monoid.minSample)
  //minSampleByTemperatureList(samples.partitions.flatMap(minSampleByTemperatureList))

  def minSampleByTemperatureList(partition: List[Sample]): Option[Sample] =
    partition.foldLeft(Option.empty[Sample]) {
      case (None, sample) => Some(sample)
      case (Some(minSample), sample) =>
        if (sample.temperatureFahrenheit < minSample.temperatureFahrenheit) Some(sample)
        else Some(minSample)
    }

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
    val (totalTemperature, length) =
      samples.parFoldMap(sample => (sample.temperatureFahrenheit, 1))(Monoid.sumDoubleInt)

    if (length == 0) None
    else Some(totalTemperature / length)
  }

  def sumTemperature(samples: ParList[Sample]): Double =
    samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble)

  /*def size(samples: ParList[Sample]): Double =
    samples.map(_ => 1).monoFoldLeft(Monoid.sumInt)*/

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  def foldLeft[From, To](parList: ParList[From], default: To)(
    combineElements: (To, From) => To,
    combineIntermediateResults: (To, To) => To
  ): To =
    parList.partitions
      .map { partition =>
        partition.foldLeft(default)(combineElements)
      }
      .foldLeft(default)(combineIntermediateResults)

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  /*def monoFoldLeft[A](parList: ParList[A], default: A)(combine: (A, A) => A): A =
    parList.partitions
      .map(_.foldLeft(default)(combine))
      .foldLeft(default)(combine)*/

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parFoldMap(Option(_))(Monoid.minSample),
      max = samples.parFoldMap(Option(_))(Monoid.maxSample),
      sum = samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble),
      size = samples.parFoldMap(_ => 1)(Monoid.sumInt)
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    samples.parFoldMap(sampleToSummary)(Summary.monoid)

  def sampleToSummary(sample: Sample): Summary =
    Summary(
      min = Some(sample),
      max = Some(sample),
      sum = sample.temperatureFahrenheit,
      size = 1
    )

  def sampleToOutput(keys: Sample => List[String])(sample: Sample): Map[String, Summary] =
    keys(sample).map(key => key -> sampleToSummary(sample)).toMap

  val monoidOutput: Monoid[Map[String, Summary]] = new Monoid[Map[String, Summary]] {
    def default: Map[String, Summary] = Map.empty[String, Summary]

    def combine(first: Map[String, Summary], second: Map[String, Summary]): Map[String, Summary] =
      second.foldLeft(first) { case (state, (city, summary)) =>
        state.updatedWith(city) {
          case None        => Some(summary)
          case Some(value) => Some(Summary.monoid.combine(value, summary))
        }
      }
  }
  def aggregateByLabel(samples: ParList[Sample])(keys: Sample => List[String]): Map[String, Summary] =
    samples.parFoldMap(sampleToOutput(keys))(monoidOutput)
}

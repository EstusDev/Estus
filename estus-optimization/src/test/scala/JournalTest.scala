package com.estus.optimization

import org.scalatest.{Matchers, FlatSpec}



class JournalTest extends FlatSpec with Matchers {

  import org.apache.spark.{SparkContext, SparkConf}
  import scala.reflect.io.Path

  val journal = Journal()
  val config = DEConfig(NP = 100)
  val request = BenchmarkFunctions(2, config).ackleyRequest
  val solution = Solution(
    objValue = Some(0.0),
    param = List(0.0),
    isFeasible = true,
    isConverged = true,
    numEval = 1000,
    status = "JournalTest",
    timeElapsed = 0)
  val conf = new SparkConf().setMaster("local").setAppName("kryoexample")
  conf.set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
  val sc = new SparkContext(conf)



  "A Journal" should
    "be able to register a record" in {
    val key = journal.registerRow(request)
    journal.keys should contain (key)
  }

  it should
    "be able to update that record" in {
    val key = journal.keys.head
    journal.retrieveRow(key).get.solution should be (None)
    journal.updateRow(key, solution)
    journal.retrieveRow(key).get.solution.get.status should be ("JournalTest")
  }

  it should
    "be able to retrieve that record" in {
    val key = journal.keys.head
    journal.retrieveRow(key).get.solution.get.status should be ("JournalTest")
  }

  it should
    "be able to remove that record" in {
    val key = journal.keys.head
    journal.removeRow(key)
    journal.keys should not contain key
  }

  it should
    "be able to purge all records" in {
    (1 to 100).foreach(_ => journal.registerRow(request))
    journal.size should be (100)
    journal.purgeJournal
    journal.size should be (0)
  }

  it should
    "be able to persist itself to file then overwrite itself with this file" in {
    val path = System.getProperty("java.io.tmpdir") +
      "/journaltest"
    Path(path).deleteRecursively()
    journal.purgeJournal
    (1 to 100).foreach(_ => journal.registerRow(request))
    journal.size should be (100)
    journal.persistJournalToFile(sc, path)
    journal.purgeJournal
    journal.size should be (0)
    journal.overwriteJournalWithFile(sc, path)
    journal.size should be (100)
    Path(path).deleteRecursively()
  }

  it should
    "be able to append additional records to itself from file" in {
    val path = System.getProperty("java.io.tmpdir") +
      "/journaltest"
    Path(path).deleteRecursively()
    journal.purgeJournal
    (1 to 50).foreach(_ => journal.registerRow(request))
    journal.size should be (50)
    journal.persistJournalToFile(sc, path)
    journal.purgeJournal
    journal.size should be (0)
    (1 to 50).foreach(_ => journal.registerRow(request))
    journal.appendJournalWithFile(sc, path)
    journal.size should be (100)
    Path(path).deleteRecursively()
  }

}

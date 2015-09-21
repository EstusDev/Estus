package com.estus.optimization

import java.io.ByteArrayOutputStream

import com.esotericsoftware.kryo.io.Input
import org.apache.hadoop.io.{Text, BytesWritable}
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.serializer.KryoSerializer

trait JournalTrait {
  // Accessors
  def size: Int
  def keys: List[String]
  def values: List[JournalNode]

  // Methods on Rows
  def registerRow (request: Request): String
  def updateRow (key: String, solution: Solution): Unit
  def retrieveRow (key: String): Option[JournalNode]
  def removeRow (key: String): Unit

  // Methods on Journal
  def persistJournalToFile (sc: SparkContext, path: String): Unit
  def overwriteJournalWithFile (sc: SparkContext, path: String): Unit
  def appendJournalWithFile (sc: SparkContext, path: String): Unit
  def purgeJournal: Unit
}

case class JournalNode (
  startTime: java.util.Date,
  request: Request,
  solution: Option[Solution] = None)

case class Journal () extends JournalTrait {

  private var journal = collection.immutable.Map.empty[String, JournalNode]

  def size: Int = journal.size

  def keys: List[String] = journal.keys.toList

  def values: List[JournalNode] = journal.values.toList

  def registerRow (request: Request): String = {
    val key = java.util.UUID.randomUUID.toString
    val stamp = new java.util.Date()
    journal = journal + (key -> JournalNode(stamp, request, None))
    key
  }

  def updateRow (key: String, solution: Solution): Unit = {
    val node = journal.get(key).get
    val stamp = node.startTime
    val request = node.request
    journal = journal - key
    journal = journal + (key -> JournalNode(stamp, request, Some(solution)))
  }

  def retrieveRow (key: String): Option[JournalNode] = {
    journal.get(key)
  }

  def removeRow (key: String): Unit = {
    journal = journal - key
  }

  def persistJournalToFile (sc: SparkContext, path: String): Unit = {
    saveAsObjectFile(journal, sc, path)
  }

  def overwriteJournalWithFile (sc: SparkContext, path: String): Unit = {
    journal = collection.immutable.Map.empty[String, JournalNode]
    val rdd = loadFromObjectFile(sc, path)
    rdd.map(x => {(x._1.toString, x._2.asInstanceOf[JournalNode])}).collect()
      .foreach(x => journal = journal + (x._1 -> x._2))
  }

  def appendJournalWithFile (sc: SparkContext, path: String): Unit = {
    var journalNew = collection.immutable.Map.empty[String, JournalNode]
    val rdd = loadFromObjectFile(sc, path)
    rdd.map(x => {(x._1.toString, x._2.asInstanceOf[JournalNode])}).collect()
      .foreach(x => journalNew = journalNew + (x._1 -> x._2))
    journal = journal ++ journalNew
  }

  def purgeJournal: Unit = {
    journal = collection.immutable.Map.empty[String, JournalNode]
  }

  private def saveAsObjectFile (
    Journal: collection.immutable.Map[String, JournalNode],
    sc: SparkContext,
    path: String): Unit = {
    val rdd = sc.makeRDD(Journal.values.toSeq)
    val kryoSerializer = new KryoSerializer(rdd.context.getConf)
    rdd.zip(sc.makeRDD(Journal.keys.toSeq)).map(row => {
      val splitArray = row._1
      val kryo = kryoSerializer.newKryo()
      val bao = new ByteArrayOutputStream()
      val output = kryoSerializer.newKryoOutput()
      output.setOutputStream(bao)
      kryo.writeClassAndObject(output, splitArray)
      output.close()
      val byteWritable = new BytesWritable(bao.toByteArray)
      (new Text(row._2), byteWritable)
    }).saveAsSequenceFile(path)
  }

  private def loadFromObjectFile (
    sc: SparkContext,
    path: String,
    MinPartitions: Int = 1): RDD[(Text, JournalNode)] = {
    val kryoSerializer = new KryoSerializer(sc.getConf)
    sc.sequenceFile(
      path,
      classOf[Text],
      classOf[BytesWritable],
      MinPartitions).map(f = x => {
      val kryo = kryoSerializer.newKryo()
      val input = new Input()
      input.setBuffer(x._2.getBytes)
      val data = kryo.readClassAndObject(input)
      val dataObject = data.asInstanceOf[JournalNode]
      (x._1, dataObject)
    })
  }

}

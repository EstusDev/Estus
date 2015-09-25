package com.estus.optimization



/*** FSM - States ***/
sealed trait State
case object InitiationState extends State
case object EvolutionDEState extends State

/*** FSM - Data ***/
final case class Data()

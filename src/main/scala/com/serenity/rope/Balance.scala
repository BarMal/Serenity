package com.serenity.rope

final case class Balance(weightBalance: Int, heightBalance: Int, leafChunkSize: Int)

object Balance:

  def default: Balance = Balance(
    weightBalance = 1000,
    heightBalance = 5,
    leafChunkSize = 1000
  )

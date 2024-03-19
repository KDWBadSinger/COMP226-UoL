book.total_volumes <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The total volume in the book.
  
  # sold
  total_ask_volume <- sum(book$ask$size)
  #sell
  total_bid_volume <- sum(book$bid$size)
  
  return(list(ask=total_ask_volume, bid=total_bid_volume))
}

book.best_prices <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   A list with "ask" and "bid", the values of which are the best prices in
    #       the book.
  
  # Lowest Ask
  best_ask_price <- min(book$ask$price)
   # Highest Bid
  best_bid_price <- max(book$bid$price)
  
  return(list(ask=best_ask_price, bid=best_bid_price))
}

book.midprice <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The midprice of the book
  .
  # LA & HB
  best_ask_price <- min(book$ask$price)
  best_bid_price <- max(book$bid$price)
  
  # Mean
  midprice <- (best_ask_price + best_bid_price) / 2
  
  return(midprice)
}

book.spread <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The spread of the book.
  
  best_ask_price <- min(book$ask$price)
  best_bid_price <- max(book$bid$price)

  spread <- best_ask_price - best_bid_price
  
  return(spread)
}

book.add <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid", "side", "price" and "size" entries.
    #
    # Returns:
    #   The updated book.
  
  # new frame
  new_order <- data.frame(
    oid = message$oid,
    price = as.numeric(message$price),
    size = as.numeric(message$size)
  )
  
  # check buy or sold 
  if (message$side == "B") {
    book$bid <- rbind(book$bid, new_order)
  } else if (message$side == "S") {
    book$ask <- rbind(book$ask, new_order)
  }
  
  return(book)
  
}

book.reduce <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid" and "amount".
    #
    # Returns:
    #   The updated book.
  
  # 遍历ask和bid，寻找与message中oid相匹配的订单
  for (side in c("ask", "bid")) {
    # 找到匹配的订单的索引
    order_index <- which(book[[side]]$oid == message$oid)
    
    # 如果找到了相应的订单
    if (length(order_index) > 0) {
      # 减少订单的大小
      book[[side]]$size[order_index] <- book[[side]]$size[order_index] - as.numeric(message$amount)
      
      # 如果订单的大小小于等于0，则将其从订单簿中移除
      if (book[[side]]$size[order_index] <= 0) {
        book[[side]] <- book[[side]][-order_index, ]
      }
      
      # 由于每个oid唯一，找到匹配的订单后就可以停止搜索
      break
    }
  }
  
  return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
    # See handout for instructions
}

book.extra2 <- function(book, size) {
    # See handout for instructions
}

book.extra3 <- function(book) {
    # See handout for instructions
}

book.extra4 <- function(book, k) {
    # See handout for instructions
}

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
  new_order <- data.frame(oid = message$oid, price = as.numeric(message$price), size = as.numeric(message$size))
  
  # Function to process the new order against the existing book
  processOrder <- function(book_side, order_side, new_order, compare_func) {
    # Find the index of the best matching order based on the compare function
    best_price_index <- which(compare_func(book_side$price, new_order$price))[1]
    
    # find a match + adjust sizes / remove orders as necessary
    while (!is.na(best_price_index) && new_order$size > 0) {
      best_order <- book_side[best_price_index, ]
      size_diff <- best_order$size - new_order$size
      if (size_diff < 0) {
        new_order$size <- -size_diff
        book_side <- book_side[-best_price_index, ]
      } else {
        book_side$size[best_price_index] <- size_diff
        if (size_diff == 0) {
          book_side <- book_side[-best_price_index, ]
        }
        new_order$size <- 0
      }
      best_price_index <- which(compare_func(book_side$price, new_order$price))[1]
    }
    
    # If there's any size left in the new order, add it to the correct side of the book
    if (new_order$size > 0) {
      order_side <- rbind(order_side, new_order)
      if (message$side == 'B') {
        order_side <- order_side[order(-order_side$price, order_side$oid), ]
      } else {
        order_side <- order_side[order(order_side$price, order_side$oid), ]
      }
    }
    return(list(book_side = book_side, order_side = order_side))
  }
  
  # Process the order based on its type and the current state of the book
  if (message$side == 'B' && nrow(book$ask) > 0 && message$price >= min(book$ask$price)) {
    results <- processOrder(book$ask, book$bid, new_order, function(a, b) a <= b)
    book$ask <- results$book_side
    book$bid <- results$order_side
  } else if (message$side == 'S' && nrow(book$bid) > 0 && message$price <= max(book$bid$price)) {
    results <- processOrder(book$bid, book$ask, new_order, function(a, b) a >= b)
    book$bid <- results$book_side
    book$ask <- results$order_side
  } else {
    if (message$side == 'B') {
      book$bid <- rbind(book$bid, new_order)
      book$bid <- book$bid[order(-book$bid$price, book$bid$oid), ]
    } else {
      book$ask <- rbind(book$ask, new_order)
      book$ask <- book$ask[order(book$ask$price, book$ask$oid), ]
    }
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
  
  for (side in c("ask", "bid")) {
    # Find the index of the order that matches the oid
    order_index <- which(book[[side]]$oid == message$oid)
    
    if (length(order_index) > 0) {
      # Decrease the size of the order
      book[[side]]$size[order_index] <- book[[side]]$size[order_index] - as.numeric(message$amount)
      
      # If the size of the order becomes less than or equal to 0, remove it from the book
      if (book[[side]]$size[order_index] <= 0) {
        book[[side]] <- book[[side]][-order_index, ]
      }
      
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
  # 订单簿=空/size=总量 return NA
  total_ask_size <- sum(book$ask$size)
  if (nrow(book$ask) == 0 || size == total_ask_size) {
    return(NA)
  }
  
  # get uni pirce
  unique_prices <- unique(book$ask$price)
  
  # init mid price
  mid_prices_after_execution <- numeric(length(unique_prices))
  
  for (i in seq_along(unique_prices)) {
    # 克隆以避免直接修改
    temp_book <- list(ask = book$ask, bid = book$bid)
    
    # 模拟添加订单
    simulated_order <- list(oid = paste0("sim", i), side = "B", size = size, price = unique_prices[i])
    temp_book_with_order_added <- book.add(temp_book, simulated_order)
    
    # 计算中间价
    mid_prices_after_execution[i] <- book.midprice(temp_book_with_order_added)
  }
  
  expected_midprice <- mean(mid_prices_after_execution)
  
  return(expected_midprice)
}


book.extra2 <- function(book, size) {
  # if size=ask订单簿的总体积，return NA
  total_ask_volume <- sum(book$ask$size)
  if (size == total_ask_volume) {
    return(NA)
  }
  
  # 价格范围：最佳ask到最高ask
  best_ask <- min(book$ask$price)
  highest_ask <- max(book$ask$price)
  
  # 生成所有可能
  possible_prices <- seq(best_ask, highest_ask, by = 1)
  
  # 计算mean
  midprices <- numeric(length = length(possible_prices))
  for (i in seq_along(possible_prices)) {
    price <- possible_prices[i]

    executed_asks <- book$ask[book$ask$price <= price, ]

    remaining_asks <- book$ask[book$ask$price > price, ]
    next_best_ask <- if (nrow(remaining_asks) > 0) min(remaining_asks$price) else price
    # 最佳bid
    best_bid <- if (nrow(book$bid) > 0) max(book$bid$price) else price
    # 中间价
    midprices[i] <- (best_bid + next_best_ask) / 2
  }
  
  # 计算中间价的期望值
  expected_midprice <- mean(midprices)
  
  return(expected_midprice)
}


book.extra3 <- function(book) {

  if (nrow(book$ask) == 0) {
    return(NA)
  }
  
  accumulated_midprice <- 0
  
  total_volume <- sum(book$ask$size)
  
  for (order_size in 1:(total_volume - 1)) {
    temp_book <- list(ask = book$ask, bid = book$bid)
    remaining_size <- order_size
    
    while (remaining_size > 0 && nrow(temp_book$ask) > 0) {
      if (temp_book$ask$size[1] <= remaining_size) {
        remaining_size <- remaining_size - temp_book$ask$size[1]
        temp_book$ask <- temp_book$ask[-1, ]
      } else {
        temp_book$ask$size[1] <- temp_book$ask$size[1] - remaining_size
        remaining_size <- 0
      }
    }
    
    # 更新累计的mp
    new_midprice <- (min(temp_book$ask$price, na.rm = TRUE) + max(temp_book$bid$price, na.rm = TRUE)) / 2
    accumulated_midprice <- accumulated_midprice + new_midprice
  }
  
  
  expected_midprice <- accumulated_midprice / (total_volume - 1)
  return(expected_midprice)
}




book.extra4 <- function(book, k) {
  # % -> 0.X
  k <- k / 100
  
  # ask订单=0，return 0
  if (length(book$ask$size) == 0) {
    return(0)
  }
  
  max_volume <- sum(book$ask$size)  # 计算总ask v
  target_midprice <- (1 + k) * book.midprice(book)  # tmp
  max_acceptable_volume <- 0  # 可接受的最大买入量 init 0
  
  for (proposed_volume in 1:(max_volume - 1)) {
    simulated_book <- book  # 创建订单簿副本
    current_size <- proposed_volume
    
    # 模拟减去当前尺寸的买单
    while (current_size > 0 && nrow(simulated_book$ask) > 0) {
      if (simulated_book$ask$size[1] <= current_size) {
        current_size <- current_size - simulated_book$ask$size[1]
        simulated_book$ask <- simulated_book$ask[-1, ]
      } else {
        simulated_book$ask$size[1] <- simulated_book$ask$size[1] - current_size
        break
      }
    }
    
    # 计算模拟买单后的mp
    new_midprice <- book.midprice(simulated_book)
    
    # mp是否超过tmp
    if (new_midprice > target_midprice || is.na(new_midprice)) {
      break  # 超过则停止
    } else {
      max_acceptable_volume <- proposed_volume  # 更新可接受的最大买入量
    }
  }
  
  return(max_acceptable_volume)
}


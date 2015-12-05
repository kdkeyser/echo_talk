//
// connection.cpp
// ~~~~~~~~~~~~~~
//
// Copyright (c) 2003-2015 Christopher M. Kohlhoff (chris at kohlhoff dot com)
//
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//

#include "connection.hpp"
#include "exit_matcher.h"
#include <vector>
#include <boost/bind.hpp>
#include <boost/logic/tribool.hpp>
#include <boost/tuple/tuple.hpp>

namespace echo {
  namespace server {

    connection::connection(boost::asio::io_service& io_service, bool should_log)
    : strand_(io_service), socket_(io_service), should_log_ (should_log), em(), total_bytes_read(0) {
    }

    boost::asio::ip::tcp::socket& connection::socket() {
      return socket_;
    }

    void connection::start() {
      if (should_log_) {
        std::cerr << "Starting connection handler" << std::endl;
      }
      
      boost::asio::ip::tcp::no_delay option (true);
      socket_.set_option(option);
      
      socket_.async_read_some(boost::asio::buffer(buffer_),
              strand_.wrap(
              boost::bind(&connection::handle_read, shared_from_this(),
              boost::asio::placeholders::error,
              boost::asio::placeholders::bytes_transferred)));
    }

    void connection::handle_read(const boost::system::error_code& e,
            std::size_t bytes_transferred) {
      if (!e) {
        total_bytes_read += bytes_transferred;
        /* Check if we got an exit command. */
        if (em.try_match(buffer_, bytes_transferred)) {
          if (should_log_) {
            std::cerr << "Saw exit: terminating connection after reading " << total_bytes_read << " bytes" << std::endl;
          }
          socket_.close();

          return;
        }

        /* Echo the data back */
        boost::asio::async_write(socket_, boost::asio::buffer(buffer_, bytes_transferred),
                strand_.wrap(
                boost::bind(&connection::handle_write, shared_from_this(),
                boost::asio::placeholders::error)));

        return;
      } else {
        if (e == boost::asio::error::eof) {
          if (should_log_) {
            std::cerr << "EOF reached" << std::endl;
          }
        } else {
          std::cerr << "Could not read data due to unknown error " << e << std::endl;
        }
        
        if (should_log_) {
          std::cerr << "Terminating connection after reading " << total_bytes_read << " bytes of data" << std::endl;
        }

        socket_.close();
        return;
      }
    }

    void connection::handle_write(const boost::system::error_code& e) {
      if (!e) {
        /* Read some more data to echo. */
        socket_.async_read_some(boost::asio::buffer(buffer_),
                strand_.wrap(
                boost::bind(&connection::handle_read, shared_from_this(),
                boost::asio::placeholders::error,
                boost::asio::placeholders::bytes_transferred)));

        return;
      } else {
        std::cerr << "Could not write data: " << e << std::endl;
        std::cerr << "Terminating connection after reading " << total_bytes_read << " bytes of data" << std::endl;
        socket_.close();
        return;
      }
    }

  }
}

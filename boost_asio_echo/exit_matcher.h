#ifndef EXIT_MATCHER_H
#define EXIT_MATCHER_H

#include <stdlib.h>
#include <boost/array.hpp>

enum class ExitState {
  OFFTRACK, NL, E, X, I, T, CR, MATCH
};

class exit_matcher {
public:
  ExitState state;

  exit_matcher();

  bool try_match(boost::array<char, 8192>& buf, size_t buf_size);
private:
  void goto_state(ExitState state);
  void trans(char c);
};

inline exit_matcher::exit_matcher() : state (ExitState::NL) {
}

inline void exit_matcher::goto_state(ExitState new_state) {
  state = new_state;
}

inline void exit_matcher::trans(char c) {
  switch (state) {
    case ExitState::NL:
      if (c == 101) { goto_state (ExitState::E); }
      break;
    case ExitState::E:
      if (c == 120) { goto_state (ExitState::X); }
      break;
    case ExitState::X:
      if (c == 105) { goto_state (ExitState::I); }
      break;
    case ExitState::I:
      if (c == 116) { goto_state (ExitState::T); }
      break;
    case ExitState::T:
      if (c == 13) { goto_state (ExitState::CR); }
      break;
    case ExitState::CR:
      if (c == 10) { goto_state (ExitState::MATCH); }
      break;
    case ExitState::MATCH:
      break;
    default:
      if (c == 10) { goto_state (ExitState::NL); }
      else { goto_state (ExitState::OFFTRACK); }
  }
  
  return;
}

inline bool exit_matcher::try_match(boost::array<char, 8192>& buf, size_t buf_size) {
  for (size_t i = 0; i < buf_size; i++) {
    char c = buf [i];
    trans (c);
  }
  
  return state == ExitState::MATCH;
}

#endif /* EXIT_MATCHER_H */


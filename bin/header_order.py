#!/usr/bin/env python

# Enforce header order in a a given file. This will only reorder in the first sequence of contiguous
# #include statements, so it will not play well with #ifdef.
#
# This attempts to enforce the guidelines at
# https://google.github.io/styleguide/cppguide.html#Names_and_Order_of_Includes
# with some allowances for Envoy-specific idioms.
#
# There is considerable overlap with what this does and clang-format's IncludeCategories (see
# https://clang.llvm.org/docs/ClangFormatStyleOptions.html). But, clang-format doesn't seem smart
# enough to handle block splitting and correctly detecting the main header subject to the Envoy
# canonical paths.

import re
import sys


def ReorderHeaders(path):
  with open(path, 'r') as f:
    source = f.read()

  all_lines = iter(source.split('\n'))
  before_includes_lines = []
  includes_lines = []
  after_includes_lines = []

  # Collect all the lines prior to the first #include in before_includes_lines.
  try:
    while True:
      line = all_lines.next()
      if line.startswith('#include'):
        includes_lines.append(line)
        break
      before_includes_lines.append(line)
  except StopIteration:
    pass

  # Collect all the #include and whitespace lines in includes_lines.
  try:
    while True:
      line = all_lines.next()
      if not line:
        continue
      if not line.startswith('#include'):
        after_includes_lines.append(line)
        break
      includes_lines.append(line)
  except StopIteration:
    pass

  # Collect the remaining lines in after_includes_lines.
  after_includes_lines += list(all_lines)

  # Filter for includes that finds the #include of the header file associated with the source file
  # being processed. E.g. if 'path' is source/common/common/hex.cc, this filter matches
  # "common/common/hex.h".
  def file_header_filter():
    return lambda f: f.endswith('.h"') and path.endswith(f[1:-3] + '.cc')

  def regex_filter(regex):
    return lambda f: re.match(regex, f)

  # Filters that define the #include blocks
  block_filters = [
      file_header_filter(),
      regex_filter('<.*\.h>'),
      regex_filter('<.*>'),
      regex_filter('"envoy/.*"'),
      regex_filter('"common/.*"'),
      regex_filter('"exe/.*"'),
      regex_filter('"server/.*"'),
      regex_filter('"test/.*"'),
  ]

  blocks = []
  already_included = set([])
  for b in block_filters:
    block = []
    for line in includes_lines:
      header = line[len('#include '):]
      if line not in already_included and b(header):
        block.append(line)
        already_included.add(line)
    if len(block) > 0:
      blocks.append(block)

  # Anything not covered by block_filters gets its own block.
  misc_headers = list(set(includes_lines).difference(already_included))
  if len(misc_headers) > 0:
    blocks.append(misc_headers)

  reordered_includes_lines = '\n\n'.join(
      ['\n'.join(sorted(block)) for block in blocks])

  if reordered_includes_lines:
    reordered_includes_lines += '\n'

  return '\n'.join(
      filter(lambda x: x, [
          '\n'.join(before_includes_lines),
          reordered_includes_lines,
          '\n'.join(after_includes_lines),
      ]))


if __name__ == '__main__':
  if len(sys.argv) == 2:
    sys.stdout.write(ReorderHeaders(sys.argv[1]))
    sys.exit(0)
  elif len(sys.argv) == 3 and sys.argv[1] == '--rewrite':
    path = sys.argv[2]
    reorderd_source = ReorderHeaders(path)
    with open(path, 'w') as f:
      f.write(reorderd_source)
    sys.exit(0)
  print 'Usage: %s [--rewrite] <source file path>' % sys.argv[0]
  sys.exit(1)



sys_extra_includes = [
      ('std::runtime_error', 'stdexcept'),
      ('strcmp', 'string.h'),
      ('strcasecmp', 'strings.h'),
      ('std::unique_ptr', 'memory'),
      ('std::shared_ptr', 'memory'),
      ('std::unordered_set', 'unordered_set'),
      ('std::transform', 'algorithm'),
      ('std::vector', 'vector'),
      ('std::regex', 'regex'),
      ('spdlog::', 'spdlog/spdlog.h'),
      ('fmt::format', 'spdlog/spdlog.h'),
      ('std::string', 'string'),
      ('uint32_t', 'cstdint'),
      ('uint64_t', 'cstdint'),
      ('std::array', 'array'),
      ('sockaddr_un', 'sys/un.h'),
      ('sockaddr_in', 'netinet/ip.h'),
      ('std::chrono', 'chrono'),
      ('std::list', 'list'),
      ('std::multimap', 'map'),
      ('std::unordered_map', 'unordered_map'),
      ('std::stringstream', 'sstream'),
      ('std::condition_variable', 'condition_variable'),
      ('std::mutex', 'mutex'),
      (' ::close', 'unistd.h'),
      ('inet_ntop', 'arpa/inet.h'),
      ('inet_pton', 'arpa/inet.h'),
      ('htonl', 'arpa/inet.h'),
      ('setsockopt', 'sys/types.h'),
      ('setsockopt', 'sys/socket.h'),
      ('TCP_NODELAY', 'netinet/tcp.h'),
      ('SIGTERM', 'signal.h'),
      ('SIGPIPE', 'signal.h'),
      ('std::function', 'functional'),
      ('std::ifstream', 'fstream'),
      ('std::ofstream', 'fstream'),
      ('std::forward_list', 'forward_list'),
      ('std::atomic', 'atomic'),
      ('std::cerr', 'iostream'),
      ('hostent', 'netdb.h'),
      (' bind(', 'sys/types.h'),
      (' bind(', 'sys/socket.h'),
      ('std::ostream', 'iostream'),
  ]

  extra_includes = [
      ('MOCK_METHOD', 'gmock/gmock.h'),
      ('MOCK_CONST_METHOD', 'gmock/gmock.h'),
      ('MATCHER_P', 'gmock/gmock.h'),
      ('ON_CALL', 'gmock/gmock.h'),
      ('using testing::_', 'gmock/gmock.h'),
      ('testing::Test', 'gtest/gtest.h'),
      ('EXPECT_', 'gtest/gtest.h'),
      ('\nTEST(', 'gtest/gtest.h'),
      ('\nTEST_F(', 'gtest/gtest.h'),
      ('\nTEST_P(', 'gtest/gtest.h'),
      ('HeaderMapImpl', 'test/test_common/printers.h'),
      ('Buffer::', 'test/test_common/printers.h'),
      ('RespValue', 'test/test_common/printers.h'),
  ]

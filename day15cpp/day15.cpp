#include <algorithm>
#include <array>
#include <cassert>
#include <iostream>
#include <iterator>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <unordered_set>
#include <variant>
#include <vector>

using namespace std;

const string_view input = R"(################################
##############.#.#...G.#########
##############.........#########
##############...#.#...#########
##############.##..#...#########
##############..........########
#############.......G...########
############G............#######
############..#G...G.....#######
#############.##.G.G.#..########
###############......##.########
#######.##G...........##########
######..G.....#####...##########
#####...#....#######..##########
#####G......#########.##########
#####.G.....#########.##########
#######..#G.#########E######..##
#.######....#########....###...#
#..G####...E#########....###..##
#.....#G.....#######.........###
##....#.......#####..........###
##.......E#........E.........###
#....G.G........#E........E.####
#........#.......E...##....#####
#........###..G............#####
##G......##......#.E......######
##.#.........##..........#######
#G.#...#G.....#.........##.#####
#####..#......#.#.....#...E#####
########........#....###.....#.#
########.........#E..#####.#...#
################################)";

struct pos {
  int x;
  int y;

  friend bool operator==(pos const& p1, pos const& p2) {
    return p1.x == p2.x && p1.y == p2.y;
  }
  friend bool operator!=(pos const& p1, pos const& p2) { return !(p1 == p2); }
};

namespace std {
  template <>
  struct hash<pos> {
    size_t operator()(pos p) const {
      return static_cast<size_t>(p.x) | (static_cast<size_t>(p.y) << 32);
    }
  };
}  // namespace std

template <class T>
class matrix {
 public:
  explicit matrix(int num_cols, int num_rows)
      : num_cols_(num_cols),
        elements_(static_cast<size_t>(num_cols) *
                  static_cast<size_t>(num_rows)) {}

  int num_cols() const { return num_cols_; }
  int num_rows() const { return static_cast<int>(size(elements_) / num_cols_); }

  T& operator[](pos p) {
    assert(p.x >= 0 && p.x < num_cols_);
    assert(p.y >= 0 && p.y < num_rows());
    return elements_[static_cast<size_t>(p.x) +
                     static_cast<size_t>(p.y) * num_cols_];
  }
  T const& operator[](pos p) const {
    return const_cast<matrix*>(this)->operator[](p);
  }

  auto begin() { return elements_.begin(); }
  auto end() { return elements_.end(); }
  auto begin() const { return elements_.begin(); }
  auto end() const { return elements_.end(); }

  template <class F, class U = remove_cvref_t<invoke_result_t<F, T const&>>>
  auto map(F&& f) const -> matrix<U> {
    matrix<U> result(num_cols(), num_rows());
    transform(begin(), end(), result.begin(), std::forward<F>(f));
    return result;
  }

  template <class P>
  auto positions_where(P&& p) const -> vector<pos> {
    vector<pos> positions;
    for (int i = 0; i < elements_.size(); ++i) {
      if (invoke(p, elements_[i])) {
        positions.push_back({i % num_cols_, i / num_cols_});
      }
    }
    return positions;
  }

 private:
  int num_cols_;
  vector<T> elements_;
};

template <
    class F,
    class T = remove_cvref_t<decltype(*declval<invoke_result_t<F, char>>())>>
auto matrix_from_lines(vector<string_view> const& lines, F&& f)
    -> optional<matrix<T>> {
  int const num_rows = static_cast<int>(size(lines));
  int const num_cols = num_rows > 0 ? static_cast<int>(size(lines[0])) : 0;
  auto m = matrix<T>(num_cols, num_rows);
  for (int y = 0; y < num_rows; ++y) {
    if (size(lines[y]) != num_cols)
      throw runtime_error("inconsistent column lengths");
    for (int x = 0; x < num_cols; ++x) {
      auto v = f(lines[y][x]);
      if (!v) return nullopt;
      m[{x, y}] = std::move(*v);
    }
  }
  return m;
}

enum class square { open, wall };

ostream& operator<<(ostream& os, square s) {
  switch (s) {
    case square::open:
      return os << '.';
    case square::wall:
      return os << '#';
  }
  return os << '?';
}

auto square_from_char(char c) -> optional<square> {
  switch (c) {
    case '.':
      return square::open;
    case '#':
      return square::wall;
  }
  return nullopt;
}

using map = matrix<square>;

enum class unit_type : char { elf, goblin };

ostream& operator<<(ostream& os, unit_type ut) {
  switch (ut) {
    case unit_type::elf:
      return os << 'E';
    case unit_type::goblin:
      return os << 'G';
  }
  return os << '?';
}

auto unit_type_from_char(char c) -> optional<unit_type> {
  switch (c) {
    case 'E':
      return unit_type::elf;
    case 'G':
      return unit_type::goblin;
  }
  return nullopt;
}

template <class F, class G>
auto alt(F&& f, G&& g) {
  return [&](auto&& x) {
    using T = remove_cvref_t<decltype(*f(x))>;
    using U = remove_cvref_t<decltype(*g(x))>;
    auto y = f(x);
    if (y) return optional<variant<T, U>>(*y);
    auto z = g(forward<decltype(x)>(x));
    if (z) return optional<variant<T, U>>(*z);
    return optional<variant<T, U>>();
  };
}

template <class F, class G>
auto optional_map(F&& f, G&& g) {
  return [&](auto&& x)
             -> optional<invoke_result_t<
                 G,
                 typename invoke_result_t<F, decltype(x)>::value_type>> {
    auto y = f(forward<decltype(x)>(x));
    if (!y) return nullopt;
    return g(move(*y));
  };
}

auto lines(string_view input) -> vector<string_view> {
  vector<string_view> lines;
  size_t i = 0;
  while (i < size(input)) {
    auto e = input.find_first_of('\n', i);
    if (e == string_view::npos) e = size(input);
    lines.push_back({&input[i], e - i});
    i = e + 1;
  }
  return lines;
}

template <class... Fs>
struct overload : Fs... {
  using Fs::operator()...;
};
template <class... Fs>
overload(Fs...)->overload<Fs...>;

auto square_or_open(variant<unit_type, square> const& v) -> square {
  return visit(overload{[](square const& s) { return s; },
                        [](auto const&) { return square::open; }},
               v);
}

auto maybe_unit_type(variant<unit_type, square> const& v)
    -> optional<unit_type> {
  return visit(overload{[](unit_type const& u) { return optional(u); },
                        [](auto const&) { return optional<unit_type>(); }},
               v);
}

struct unit {
  unit_type type;
  int hitpoints = 200;
};

auto make_unit(unit_type type) -> unit { return unit{type}; }

auto attack_power(unit const& u, int elf_attack_power) -> int {
  return u.type == unit_type::elf ? elf_attack_power : 3;
}

using units = matrix<optional<unit>>;

auto parse_map(string_view input) -> optional<tuple<map, units>> {
  auto const mat_square_and_units = matrix_from_lines(
      lines(input), alt(unit_type_from_char, square_from_char));
  if (!mat_square_and_units) return nullopt;
  auto map = mat_square_and_units->map(square_or_open);
  auto units = mat_square_and_units->map([](auto const& v) {
    auto u = maybe_unit_type(v);
    if (!u) return optional<unit>();
    return optional(make_unit(*u));
  });
  return {{map, units}};
}

auto dump_state(ostream& os, map const& m, units const& us) {
  for (int y = 0; y < m.num_rows(); ++y) {
    vector<unit> units_in_row;
    for (int x = 0; x < m.num_cols(); ++x) {
      auto const p = pos{x, y};
      if (us[p]) {
        os << us[p]->type;
        units_in_row.push_back(*us[p]);
      } else {
        os << m[p];
      }
    }
    os << "   ";
    bool first = true;
    for (unit const& u : units_in_row) {
      if (!exchange(first, false)) {
        os << ", ";
      }
      os << u.type << "(" << u.hitpoints << ")";
    }
    os << "\n";
  }
}

auto is_enemy_of(unit const& u1) {
  return [&](optional<unit> const& u2) { return u2 && u1.type != u2->type; };
}

template <class T, int N>
class fixed_vector {
 public:
  fixed_vector() : data_(), size_(0) {}

  template <class... E>
  fixed_vector(E&&... elements)
      : data_{forward<E>(elements)...}, size_(sizeof...(elements)) {
    static_assert(sizeof...(elements) <= N);
  }

  size_t empty() const { return size_ == 0; }

  auto begin() { return data_.begin(); }
  auto end() { return data_.begin() + size_; }
  auto begin() const { return data_.begin(); }
  auto end() const { return data_.begin() + size_; }

  T& push_back(T const& x) {
    assert(size_ < N);
    return data_[size_++] = x;
  }

  T& push_back(T&& x) {
    assert(size_ < N);
    return data_[size_++] = move(x);
  }

 private:
  array<T, N> data_;
  size_t size_;
};

using adj_matrix_t = matrix<fixed_vector<pos, 4>>;

auto calc_adj_matrix(map const& m) -> adj_matrix_t {
  adj_matrix_t adj_matrix(m.num_cols(), m.num_rows());
  for (int y = 0; y < m.num_rows(); ++y) {
    for (int x = 0; x < m.num_cols(); ++x) {
      auto p = pos{x, y};
      auto& adj = adj_matrix[p];
      if (p.x < m.num_cols() - 1) adj.push_back({p.x + 1, p.y});
      if (p.x > 0) adj.push_back({p.x - 1, p.y});
      if (p.y < m.num_rows() - 1) adj.push_back({p.x, p.y + 1});
      if (p.y > 0) adj.push_back({p.x, p.y - 1});
    }
  }
  return adj_matrix;
}

auto adjacent_positions(map const& m, vector<pos>& pos_buffer, pos p)
    -> vector<pos>& {
  vector<pos>& adj = pos_buffer;
  adj.clear();
  adj.reserve(4);
  if (p.x < m.num_cols() - 1) adj.push_back({p.x + 1, p.y});
  if (p.x > 0) adj.push_back({p.x - 1, p.y});
  if (p.y < m.num_rows() - 1) adj.push_back({p.x, p.y + 1});
  if (p.y > 0) adj.push_back({p.x, p.y - 1});
  return adj;
}

auto adjacent_open_or_position(map const& map,
                               units const& units,
                               vector<pos> const& positions,
                               pos current_pos) -> vector<pos> {
  vector<pos> adjacent;
  vector<pos> adj;
  for (pos const& position : positions) {
    adjacent_positions(map, adj, position);
    copy_if(adj.begin(),
            adj.end(),
            inserter(adjacent, adjacent.end()),
            [&](pos const& p) {
              return p == current_pos ||
                     (map[p] == square::open && !units[p].has_value());
            });
  }
  return adjacent;
}

auto adjacent_open_positions(map const& m,
                             adj_matrix_t const& am,
                             units const& us) {
  return [&](vector<pos>& pos_buffer, pos p) -> vector<pos>& {
    auto& adj = pos_buffer;
    adj.clear();
    copy_if(am[p].begin(), am[p].end(), back_inserter(adj), [&](pos adjp) {
      return m[adjp] == square::open && !us[adjp].has_value();
    });
    return adj;
  };
}

struct label_data {
  int distance;
  fixed_vector<pos, 4> nexts;
};

using label = optional<label_data>;

template <class AdjF, class LabelContainer>
auto& label_from(AdjF&& adjf,
                 LabelContainer& labels,
                 vector<pos>& pos_buffer,
                 pos p) {
  // vector<pos> newps;
  label& l = labels[p];
  if (!l) l = label_data{0, {}};
  auto d = l->distance + 1;
  auto& adj = adjf(pos_buffer, p);
  // newps.reserve(adj.size());
  adj.erase(remove_if(adj.begin(),
                      adj.end(),
                      [&labels, p, d](pos adjp) {
                        label& adjl = labels[adjp];
                        if (!adjl) {
                          adjl = label_data{d, {p}};
                          return false;
                        } else if (adjl->distance > d) {
                          assert(false);
                        } else if (adjl->distance == d) {
                          adjl->nexts.push_back(p);
                        }
                        return true;
                      }),
            adj.end());
  // for (pos adjp : adj) {
  //  label& adjl = labels[adjp];
  //  if (!adjl) {
  //    adjl = label_data{d, {p}};
  //    newps.push_back(adjp);
  //  } else if (adjl->distance > d) {
  //    assert(false);
  //  } else if (adjl->distance == d) {
  //    adjl->nexts.push_back(p);
  //  }
  //}
  // return newps;
  return adj;
}

#include <queue>

template <class AdjF, class LabelContainer>
auto shortest_paths_to(AdjF&& adjf,
                       pos from,
                       vector<pos> const& tos,
                       LabelContainer& labels) {
  queue<pos> todo;
  vector<pos> pos_buffer;

  todo.push(from);
  while (!todo.empty()) {
    pos p = todo.front();
    todo.pop();
    for (pos newp :
         label_from(std::forward<AdjF>(adjf), labels, pos_buffer, p)) {
      todo.push(newp);
    }
  }
}

template <class LabelContainer>
auto follow_nexts_until_distance(LabelContainer const& labels, pos p, int d)
    -> vector<pos> {
  label const& l = labels[p];
  if (!l.has_value()) return {};
  vector<pos> nexts = {p};
  auto distance = l->distance;
  std::vector<pos> next_nexts;
  while (distance > d) {
    next_nexts.clear();
    for (pos next : nexts) {
      label const& l = labels[next];
      assert(l);
      assert(l->distance == distance);
      assert(!l->nexts.empty());
      for (pos n : l->nexts) {
        if (find(next_nexts.begin(), next_nexts.end(), n) == next_nexts.end()) {
          next_nexts.push_back(n);
        }
      }
    }
    nexts.assign(next_nexts.begin(), next_nexts.end());
    --distance;
  }
  return nexts;
}

auto compare_reading_order(pos const& lhs, pos const& rhs) -> bool {
  return tie(lhs.y, lhs.x) < tie(rhs.y, rhs.x);
}

auto compare_distance_then_reading_order(tuple<pos, int> const& lhs,
                                         tuple<pos, int> const& rhs) -> bool {
  auto const& [lp, ld] = lhs;
  auto const& [rp, rd] = rhs;
  return ld < rd || ld == rd && compare_reading_order(lp, rp);
}

auto move(map const& map,
          adj_matrix_t const& adj_matrix,
          units const& units,
          pos current_pos) -> optional<pos> {
  unit const& current_unit = units[current_pos].value();
  auto target_positions = units.positions_where(is_enemy_of(current_unit));
  if (target_positions.empty()) {
    return nullopt;
  }
  auto in_range_positions =
      adjacent_open_or_position(map, units, target_positions, current_pos);
  auto labels = matrix<label>(map.num_cols(), map.num_rows());
  shortest_paths_to(adjacent_open_positions(map, adj_matrix, units),
                    current_pos,
                    in_range_positions,
                    labels);
  vector<tuple<pos, int>> positions_and_distances_in_range;
  positions_and_distances_in_range.reserve(in_range_positions.size());
  for (pos p : in_range_positions) {
    if (labels[p].has_value()) {
      positions_and_distances_in_range.push_back({p, labels[p]->distance});
    }
  }
  auto nearest_in_range = min_element(positions_and_distances_in_range.begin(),
                                      positions_and_distances_in_range.end(),
                                      compare_distance_then_reading_order);
  if (nearest_in_range == positions_and_distances_in_range.end()) {
    return current_pos;
  }
  vector<pos> nexts =
      follow_nexts_until_distance(labels, get<pos>(*nearest_in_range), 1);
  auto best_next_label =
      min_element(nexts.begin(), nexts.end(), compare_reading_order);
  if (best_next_label == nexts.end()) {
    return current_pos;
  }
  return *best_next_label;
}

auto adjacent_targets(map const& m, units const& us, pos p) -> vector<pos> {
  vector<pos> targets;
  adjacent_positions(m, targets, p);
  targets.erase(
      remove_if(targets.begin(),
                targets.end(),
                [&](pos adjp) { return !is_enemy_of(*us[p])(us[adjp]); }),
      targets.end());
  return targets;
}

auto attack(map const& m, units& us, pos p, int elf_attack_power) {
  vector<pos> target_positions = adjacent_targets(m, us, p);
  auto weakest =
      min_element(target_positions.begin(),
                  target_positions.end(),
                  [&](pos const& lhs, pos const& rhs) {
                    return us[lhs]->hitpoints < us[rhs]->hitpoints ||
                           us[lhs]->hitpoints == us[rhs]->hitpoints &&
                               compare_reading_order(lhs, rhs);
                  });
  if (weakest == target_positions.end()) return;
  optional<unit>& weakest_unit = us[*weakest];
  if ((weakest_unit->hitpoints -= attack_power(*us[p], elf_attack_power)) <=
      0) {
    weakest_unit = nullopt;
  }
}

auto turn(map const& m,
          adj_matrix_t const& am,
          units& us,
          pos p,
          int elf_attack_power) {
  if (!us[p]) return true;  // the unit might have been killed before its turn
  optional<pos> new_pos = move(m, am, us, p);
  if (!new_pos) return false;
  if (*new_pos != p) {
    assert(!us[*new_pos]);
    assert(m[*new_pos] == square::open);
    swap(us[p], us[*new_pos]);
  }
  attack(m, us, *new_pos, elf_attack_power);
  return true;
}

auto round(map const& m,
           adj_matrix_t const& am,
           units& us,
           int elf_attack_power) {
  auto ps = us.positions_where(&optional<unit>::has_value);
  assert(is_sorted(ps.begin(), ps.end(), compare_reading_order));
  for (pos const& p : ps) {
    if (!turn(m, am, us, p, elf_attack_power)) return false;
  }
  return true;
}

auto battle(map const& m, units& us, int elf_attack_power) {
  auto am = calc_adj_matrix(m);
  int num_rounds = 0;
  do {
    // cout << "Round " << num_rounds << endl;
    // dump_state(cout, m, us);
    if (!round(m, am, us, elf_attack_power)) break;
    ++num_rounds;
  } while (true);
  return num_rounds;
}

#include <numeric>

auto outcome(int num_rounds, units const& us) {
  int sum_hp =
      std::reduce(us.begin(), us.end(), 0, [](int hp, optional<unit> const& u) {
        return hp + (u ? u->hitpoints : 0);
      });
  cout << "rounds = " << num_rounds << "\n";
  cout << "sum_hp = " << sum_hp << "\n";
  return num_rounds * sum_hp;
}

auto num_elves(units const& us) {
  return count_if(us.begin(), us.end(), [](optional<unit> const& ou) {
    return ou && ou->type == unit_type::elf;
  });
}

auto test_input = R"(#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######)";

auto main() -> int {
  auto map = parse_map(input);
  assert(map);
  auto const& [m, orig_us] = *map;
  auto start_num_elves = num_elves(orig_us);
  int elf_attack_power = 3;
  for (;;) {
    cout << "Elf attack power: " << elf_attack_power << "\n";
    auto us = orig_us;
    int num_rounds = battle(m, us, elf_attack_power);
    auto end_num_elves = num_elves(us);
    cout << "Elves " << start_num_elves << " -> " << end_num_elves << "\n";
    // dump_state(cout, m, us);
    cout << outcome(num_rounds, us) << "\n";
    if (end_num_elves == start_num_elves) break;
    ++elf_attack_power;
  }
}
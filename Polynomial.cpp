#include <cmath>
#include <iostream>
#include <map>
#include <utility>
#include <vector>

template<typename T>
class Polynomial {
 public:
    using const_iterator = typename std::map<int, T>::const_iterator;
    using const_reverse_iterator = typename std::map<int, T>::const_reverse_iterator;
    using iterator = typename std::map<int, T>::iterator;
    using reverse_iterator = typename std::map<int, T>::reverse_iterator;

    Polynomial(const std::vector<T>& coefficients) {
        for (size_t i = 0; i != coefficients.size(); ++i) {
            if (coefficients[i] != T()) {
                Data.insert({i, coefficients[i]});
            }
        }
    }

    Polynomial(const T& monom = T(), int power = 0) {
        if (monom != T()) {
            Data.insert({power, monom});
        }
    }

    template<typename It>
    Polynomial(It first, It last) {
        int power = 0;
        while (first != last) {
            if (*first != T()) {
                Data.insert({power, *first});
            }
            ++first;
            ++power;
        }
    }

    const_iterator begin() const {
        return Data.begin();
    }

    const_iterator end() const {
        return Data.end();
    }

    iterator begin() {
        return Data.begin();
    }

    iterator end() {
        return Data.end();
    }

    const_reverse_iterator rbegin() const {
        return Data.rbegin();
    }

    const_reverse_iterator rend() const {
        return Data.rend();
    }

    reverse_iterator rbegin() {
        return Data.rbegin();
    }

    reverse_iterator rend() {
        return Data.rend();
    }

    Polynomial& operator += (const Polynomial& other) {
        for (const auto& monom : other) {
            Data[monom.first] += monom.second;
        }
        normalize();
        return *this;
    }

    Polynomial& operator -= (const Polynomial& other) {
        for (const auto& monom : other) {
            Data[monom.first] -= monom.second;
        }
        normalize();
        return *this;
    }

    Polynomial& operator *= (const Polynomial& other) {
        Polynomial tmp;
        for (const auto& it1 : *this) {
            for (const auto& it2 : other) {
                int power = it1.first + it2.first;
                const auto& factor = it1.second * it2.second;
                tmp.Data[power] += factor;
            }
        }
        tmp.normalize();
        *this = tmp;
        return *this;
    }

    T operator[] (int power) const {
        auto it = Data.find(power);
        return it == Data.end() ? T() : it->second;
    }

    T operator() (const T& value = T()) const {
        T res = T();
        int power = Degree();
        auto rit = Data.rbegin();
        if (power == 0) {
            return rit->second;
        }

        while (power > 0) {
            if (rit == Data.rend()) {
                while (power > 0) {
                    res *= value;
                    --power;
                }
            } else {
                while (power > rit->first) {
                    res *= value;
                    --power;
                }
                res += rit->second;
                ++rit;
            }
        }
        return res;
    }

    int Degree() const {
        return Data.empty() ? -1 : Data.rbegin()->first;
    }

    friend bool operator == (const Polynomial& left, const Polynomial& right) {
        return left.Data == right.Data;
    }

    friend bool operator != (const Polynomial& left, const Polynomial& right) {
        return !(left == right);
    }

    friend Polynomial operator + (Polynomial left, const Polynomial& right) {
        return left += right;
    }

    friend Polynomial operator - (Polynomial left, const Polynomial& right) {
        return left -= right;
    }

    friend Polynomial operator * (Polynomial left, const Polynomial& right) {
        return left *= right;
    }

    friend Polynomial operator / (const Polynomial& left, const Polynomial& right) {
        auto ans = divide(left, right);
        return ans.first;
    }

    friend Polynomial operator % (const Polynomial& left, const Polynomial& right) {
        auto ans = divide(left, right);
        return ans.second;
    }

    friend Polynomial operator , (Polynomial left, Polynomial right) {
        while (right.Degree() != -1) {
            auto rem = left % right;
            left = right;
            right = rem;
        }
        if (left.Degree() != -1) {
            auto coef = left[left.Degree()];
            for (auto& it : left)
                it.second /= coef;
        }
        return left;
    }

 private:
    std::map<int, T> Data;

    void normalize() {
        for (auto it = Data.begin(); it != Data.end(); ) {
            if (it->second == T()) {
                it = Data.erase(it);
            } else {
                ++it;
            }
        }
    }

    static std::pair<Polynomial, Polynomial> divide(Polynomial left, const Polynomial& right) {
        Polynomial res, quot;
        while (left.Degree() >= right.Degree()) {
            quot = {left[left.Degree()] / right[right.Degree()], left.Degree() - right.Degree()};
            res += quot;
            left -= right * quot;
        }
        return std::make_pair(res, left);  // (quotient, remainder)
    }
};

template<typename T>
void print_monom(
    std::ostream& out,
    const std::pair<const int, T>& monom,
    bool first_monom = false) {
    if (!first_monom && monom.second > 0) {
        out << '+';
    }
    if (monom.second < 0) {
        out << '-';
    }
    if (monom.first == 0 || abs(monom.second) != 1) {
        out << abs(monom.second);
    }
    if (monom.first > 0) {
        if (abs(monom.second) != 1) {
            out << '*';
        }
        out << 'x';
        if (monom.first > 1) {
            out << '^' << monom.first;
        }
    }
}

template<typename T>
std::ostream& operator << (std::ostream& out, const Polynomial<T>& polynomial) {
    if (polynomial.Degree() == -1) {
        out << 0;
    } else {
        auto rit = polynomial.rbegin();
        print_monom(out, *rit, true);
        ++rit;
        while (rit != polynomial.rend()) {
            print_monom(out, *rit++);
        }
    }
    return out;
}

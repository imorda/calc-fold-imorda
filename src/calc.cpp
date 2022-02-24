#include "calc.h"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr

namespace {

const std::size_t max_decimal_digits = 10;

enum class Op
{
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
    // error
    case Op::ERR: return 0;
    // unary
    case Op::NEG: return 1;
    case Op::SQRT: return 1;
    // binary
    case Op::SET: return 2;
    case Op::ADD: return 2;
    case Op::SUB: return 2;
    case Op::MUL: return 2;
    case Op::DIV: return 2;
    case Op::REM: return 2;
    case Op::POW: return 2;
    }
}

Op parse_op(const std::string & line, std::size_t & i, bool & fold)
{
    const auto rollback = [&i, &line, &fold](const std::size_t n) {
        if (fold) {
            i--;
        }
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };

    // Returns ret if fold operation is correct, otherwise Op::ERR
    const auto validate_fold = [&i, &line, &fold](const Op ret) {
        if (fold && (i >= line.size() || line[i++] != ')')) {
            std::cerr << "Incorrect folded operation specified " << line << std::endl;
            return Op::ERR;
        }
        return ret;
    };

    if (line[i] == '(') {
        fold = true;
        i++;
    }
    switch (line[i++]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        --i; // a first digit is a part of op's argument
        return validate_fold(Op::SET);
    case '+':
        return validate_fold(Op::ADD);
    case '-':
        return validate_fold(Op::SUB);
    case '*':
        return validate_fold(Op::MUL);
    case '/':
        return validate_fold(Op::DIV);
    case '%':
        return validate_fold(Op::REM);
    case '_':
        return validate_fold(Op::NEG);
    case '^':
        return validate_fold(Op::POW);
    case 'S':
        switch (line[i++]) {
        case 'Q':
            switch (line[i++]) {
            case 'R':
                switch (line[i++]) {
                case 'T':
                    return validate_fold(Op::SQRT);
                default:
                    return rollback(4);
                }
            default:
                return rollback(3);
            }
        default:
            return rollback(2);
        }
    default:
        return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

bool parse_arg(const std::string & line, std::size_t & i, double & res, const bool fold)
{
    res = 0;
    std::size_t count = 0;
    bool good = true;
    bool ongoing = true;
    bool integer = true;
    double fraction = 1;
    while (ongoing && good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (integer) {
                res *= 10;
                res += line[i] - '0';
            }
            else {
                fraction /= 10;
                res += (line[i] - '0') * fraction;
            }
            ++i;
            ++count;
            break;
        case '.':
            integer = false;
            ++i;
            break;
        case ' ':
            ongoing = false;
            if (fold) {  // Unit tests only accept trailing whitespaces in folding operations (intended?)
                break;
            }
        default:
            good = false;
            break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
        return false;
    }
    else if (i < line.size() && count >= max_decimal_digits) {
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
        return false;
    }
    return true;
}

double unary(const double current, const Op op)
{
    switch (op) {
    case Op::NEG:
        return -current;
    case Op::SQRT:
        if (current > 0) {
            return std::sqrt(current);
        }
        else {
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            [[fallthrough]];
        }
    default:
        return current;
    }
}

bool n_ary(const Op op, double & left, const double right)
{
    switch (op) {
    case Op::SET:
        left = right;
        return true;
    case Op::ADD:
        left = left + right;
        return true;
    case Op::SUB:
        left = left - right;
        return true;
    case Op::MUL:
        left = left * right;
        return true;
    case Op::DIV:
        if (right != 0) {
            left = left / right;
            return true;
        }
        else {
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return false;
        }
    case Op::REM:
        if (right != 0) {
            left = std::fmod(left, right);
            return true;
        }
        else {
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return false;
        }
    case Op::POW:
        left = std::pow(left, right);
        return true;
    default:
        return false;
    }
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    bool fold = false;
    const auto op = parse_op(line, i, fold);

    switch (arity(op)) {
    case 2: {
        bool error = false;
        int arg_counter = 0;
        double newValue = current;
        do {
            i = skip_ws(line, i);
            auto old_i = i;
            double arg;
            const bool success = parse_arg(line, i, arg, fold);
            if (i == old_i) {
                if (fold && i >= line.size() && arg_counter >= 1) {  // Trailing whitespaces are ok if there is at least 1 argument
                    break;
                }
                std::cerr << "No argument for a binary operation" << std::endl;
                error = true;
                break;
            }
            else if (!success) {
                error = true;
                break;
            }
            arg_counter++;
            bool res = n_ary(op, newValue, arg);
            if (!res) {
                error = true;
                break;
            }
        } while (fold && i < line.size());

        if (error) {
            break;
        }
        return newValue;
    }
    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(current, op);
    }
    default: break;
    }
    return current;
}

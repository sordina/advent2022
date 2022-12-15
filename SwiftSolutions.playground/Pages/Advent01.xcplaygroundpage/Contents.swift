import Foundation
import CoreFoundation
import SwiftSolutions_Sources

let string = try getPath("data/Advent01/input")

let paragraphs = string.components(separatedBy: "\n\n")

let lists = paragraphs.map { $0.components(separatedBy: "\n").map { Int($0) ?? 0 } }

let totals = lists.map { $0.reduce(0, { $0 + $1 }) }

let advent01 = totals.reduce(0, {max($0, $1)})

print("Advent01: \(advent01)")

let advent01b = totals.sorted()[0...2].reduce(0, {$0 + $1})

print("Advent01b: \(advent01b)")

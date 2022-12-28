//: [Previous](@previous)

import Foundation
import SwiftUI
import UIKit
import PlaygroundSupport


var greeting = "Hello, playground"

let elipse = Ellipse()
    .fill(.blue)
    .frame(width: 50, height: 50)

let view = Text("hello")
    .font(.system(size:55))

PlaygroundPage.current.setLiveView(view)

UserDefaults.standard.set("asdf", forKey: "asdf")
let foo = UserDefaults.standard.data(forKey: "asdf")
foo


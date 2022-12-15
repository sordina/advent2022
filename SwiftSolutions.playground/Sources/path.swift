import Foundation

public func getPath(_ s: String) throws -> String {
    let tokenPath = Bundle.main
        .url(forResource: "Path", withExtension: "txt")!
        .resolvingSymlinksInPath()
    let string = try String(
        contentsOf: tokenPath,
        encoding: String.Encoding.utf8
    ).trimmingCharacters(in: .whitespacesAndNewlines)
    
    return try String(contentsOfFile: "\(string)/\(s)")
}



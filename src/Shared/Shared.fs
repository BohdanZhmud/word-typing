namespace Shared

type Score = {
    name: string
    value: float
    gameId: string
}

type GameReplay = {
    score: Score
    words: string list
    round: int
    gameId: string
}

type Round = {
    words: string list
    framesPerSecond: int
    speed: float
    number: int
}

module Constants =
    let guestName = "Guest"
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
}

module Constants =
    let guestName = "Guest"
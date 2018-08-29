namespace Shared

type Score = {
    userId: string
    gameId: string
    value: float
}


type GameType = 
    | UsualGame
    | RestartLastRound

type GameReplay = {
    score: Score
    words: string list
    round: int
    gameType: GameType
}

type Round = {
    words: string list
    framesPerSecond: int
    speed: float
    number: int
}

module Constants =
    let guestName = "Guest"
    let percentageChargeForRestart = 0.1

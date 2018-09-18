namespace Shared

type AuthState =
    | NotAuthenticated
    | Authenticated

type User = {
    id: string
    displayName: string
    authState: AuthState
}

type Score = {
    userId: string
    gameId: string
    value: float
}

type RatingEntry = {
    score: Score
    userDisplayName: string
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

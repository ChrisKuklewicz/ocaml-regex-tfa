type contData = { cHistory : History.history; cAt : CorePattern.coreQ; }
type contMap = contData SimStep.HistMap.t
val simCont :
  ?prevIn:Common.strIndex * ReadPattern.uchar ->
  CorePattern.coreResult -> SimStep.simFeed
val uWrapCont : CorePattern.coreResult -> ReadPattern.ustring -> SimStep.o
val wrapSimCont : ReadPattern.ustring -> ReadPattern.ustring -> SimStep.o

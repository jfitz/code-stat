* PRC.PRG Application Procedures

* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
PROC events
DO make_ntx WITH [events],[events1],[DTOS(date)+whom]
DO make_ntx WITH [events],[events2],[whom+DTOS(date)]
DO open_file WITH [events],[events1],[events2]
RETURN

PROC whom
DO make_ntx WITH [whom],[whom],[whom]
DO open_file WITH [whom],[whom]
RETURN

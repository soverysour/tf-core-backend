default: ghcid

ghcid-exe:
	-@clear
	-@ghcid --command="stack ghci core-backend:exe:core-backend-exe"

ghcid:
	-@clear
	-@ghcid

profile:
	-@clear
	-@stack build --profile
	-@stack exec --profile -- core-backend-exe +RTS -p -hc -i0.01
	-@stack exec -- hp2ps -c core-backend-exe.hp && ps2pdf core-backend-exe.ps
	-@mv core-backend-exe.prof profiling-data/prof/`date -Iminutes`.prof
	-@mv core-backend-exe.hp profiling-data/heap-prof/`date -Iminutes`.hp
	-@mv core-backend-exe.pdf profiling-data/heap-vis/`date -Iminutes`.pdf
	-@profiteur profiling-data/prof/`date -Iminutes`.prof
	-@mv profiling-data/prof/`date -Iminutes`.prof.html profiling-data/prof-vis/`date -Iminutes`.prof.html
	-@rm core-backend-exe.aux
	-@rm core-backend-exe.ps

clean-profile:
	-@rm profiling-data/heap-prof/*
	-@rm profiling-data/heap-vis/*
	-@rm profiling-data/prof/*
	-@rm profiling-data/prof-vis/*

run:
	-@stack run core-backend-exe

swagger:
	-@stack run swagger-spec-exe

trace:
	-@stack test --trace --coverage

test:
	-@stack test --coverage

doc:
	-@stack haddock

clean:
	-@stack clean
	-@clear

.PHONY: default test run clean bench profile doc run-spec swagger ghcid ghcid-exe

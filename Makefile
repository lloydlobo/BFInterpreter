.PHONY: build test clean publish release

build:
	dotnet build -c Release

test:
	dotnet test --logger "console;verbosity=detailed"

clean:
	dotnet clean
	rm -rf publish/

publish: build
	dotnet publish -c Release -r win-x64 --self-contained true -o ./publish/win-x64
	dotnet publish -c Release -r linux-x64 --self-contained true -o ./publish/linux-x64
	dotnet publish -c Release -r osx-x64 --self-contained true -o ./publish/osx-x64

release: test publish

docker-build:
	docker build -t bfinterpreter .

docker-run:
	docker run -it bfinterpreter:latest

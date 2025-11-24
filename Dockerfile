FROM mcr.microsoft.com/dotnet/sdk:10.0
WORKDIR /app
COPY ["BFInterpreter.fsproj", "./"]
RUN dotnet restore "./BFInterpreter.fsproj"
COPY . .
ENTRYPOINT ["dotnet", "run", "--project", "BFInterpreter.fsproj", "--"]
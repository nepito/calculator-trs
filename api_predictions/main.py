from os import listdir
import json
from fastapi import FastAPI, Body


def read_json(file_name):
    f = open(
        file_name,
    )
    league = json.load(f)
    f.close()
    return league

app = FastAPI()  # pragma: no mutate


LEAGUE = {
    "inglaterra": "39",
    "francia": "61",
    "alemania": "78",
    "holanda": "88",
    "portugal": "94",
    "italia": "135",
    "espagna": "140",
}


@app.get("/{league_season_round}")
def read_root(league_season_round):
    predictions = read_json(f"data/predictions_{league_season_round}.json")
    return predictions


@app.get("/liga/{pais}")
def last_predictions(pais):
    league = LEAGUE[pais]
    r = max([f.split("_")[-1].split(".")[0] for f in listdir("data/") if f.split("_")[1] == league])
    predictions = read_json(f"data/predictions_{league}_2021_{r}.json")
    return predictions
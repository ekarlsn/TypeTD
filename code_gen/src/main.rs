use inflector::Inflector;

#[derive(PartialEq)]
enum AssetType {
    Texture,
    Sound,
}

struct Asset {
    t: AssetType,
    name: String,
    path: String,
}

fn make_asset(path: &str, name: &str, t: AssetType) -> Asset {
    Asset {
        t,
        name: name.to_string(),
        path: path.to_string(),
    }
}

type Input = Vec<Asset>;

fn main() {
    let input: Input = vec![
        make_asset("monster/zombie_1", "Troll", AssetType::Texture),
        make_asset("monster/zombie_2", "Zombie2", AssetType::Texture),
        make_asset("monster/zombie_3", "Zombie3", AssetType::Texture),
        make_asset("monster/zombie_4", "Zombie4", AssetType::Texture),
        make_asset("monster/zombie_5", "Zombie5", AssetType::Texture),
        make_asset("monster/zombie_6", "Zombie6", AssetType::Texture),
        make_asset("monster/zombie_7", "Zombie7", AssetType::Texture),
        make_asset("monster/zombie_8", "Zombie8", AssetType::Texture),
        make_asset("monster/zombie_9", "Zombie9", AssetType::Texture),
        make_asset("monster/zombie_10", "Zombie10", AssetType::Texture),
        make_asset("map/map1", "Map1", AssetType::Texture),
        make_asset("map/map2", "Map2", AssetType::Texture),
        make_asset("map/map3", "Map3", AssetType::Texture),
        make_asset("tower/archer", "TowerFreezer", AssetType::Texture),
        make_asset("tower/basic", "TowerSimplifier", AssetType::Texture),
        make_asset("tower/arrow", "Arrow", AssetType::Texture),
        make_asset("ui/main_menu", "MainMenu", AssetType::Texture),
        make_asset("ui/select_level", "SelectLevel", AssetType::Texture),
        make_asset("ui/level_finished", "LevelFinished", AssetType::Texture),
        make_asset("ui/infobox", "InfoBox", AssetType::Texture),
        make_asset("ui/ui", "Ui", AssetType::Texture),
        make_asset("ui/help_screen", "HelpScreen", AssetType::Texture),
        make_asset("text_shield", "TextShield", AssetType::Texture),
        make_asset("wakka", "MainMenuBgSound", AssetType::Sound),
        make_asset("level_failed", "LevelFailed", AssetType::Sound),
        make_asset("level_success", "LevelSuccess", AssetType::Sound),
        make_asset("zombie_die", "ZombieDie", AssetType::Sound),
        make_asset("freezer_fire", "FreezerFire", AssetType::Sound),
        make_asset("freezer_hit", "FreezerHit", AssetType::Sound),
    ];

    let sep = "\n\n\n";

    let output = [
        imports(),
        key_type_declaration(&input),
        texture_type_container(&input),
        initial_builder(&input),
        try_build(&input),
        insert_fn(&input),
        load_all(&input),
        getter(&input),
    ]
    .join(sep);
    println!("{output}");
}

fn getter(input: &Input) -> String {
    r#"getTexture : AssetPack -> TextureKey -> Texture
getTexture tp id =
    case id of
        "#
    .to_owned()
        + &filter_map_join(
            input,
            AssetType::Texture,
            |a| a.name.clone() + " -> tp." + &a.name.to_camel_case(),
            "\n        ",
        )
        + "\n"
        + r#"getSound : SoundKey -> AssetPack -> Audio.Source
getSound key soundPack =
    case key of
        "#
        + &filter_map_join(
            input,
            AssetType::Sound,
            |a| a.name.clone() + " -> soundPack." + &a.name.to_camel_case(),
            "\n        ",
        )
}

fn load_all(input: &Input) -> String {
    r#"type LoadTexture = LoadTexture String TextureKey

loadAllTextures : List LoadTexture
loadAllTextures =
    ["#.to_owned()
        + &filter_map_join(input, AssetType::Texture, |a| "(\"".to_owned() + &a.path + "\"," + &a.name + ")", "\n        ,")
        + "] |> List.map (\\( name, key ) -> LoadTexture (\"./assets/img/\" ++ name ++ \".png\") key)"
        + "\n"
        +
    r#"type LoadAudio  = LoadAudio SoundKey String

loadAllSounds : List LoadAudio
loadAllSounds =["#
        + &filter_map_join(input, AssetType::Sound, |a| "(".to_owned() + &a.name + ",\"" + &a.path + "\")", "\n        ,")
        + "\n"
        + r#"]|> List.map (\( key, filename ) -> LoadAudio key ("./assets/sound/" ++ filename ++ ".mp3"))"#
}

fn insert_fn(input: &Input) -> String {
    r#"insertTexture : TextureKey -> Texture -> AssetPackBuilder -> AssetPackBuilder
insertTexture id t tpb =
    case id of
        "#
    .to_string()
        + &filter_map_join(
            input,
            AssetType::Texture,
            |a| a.name.to_string() + "->{tpb|" + &a.name.to_camel_case() + "=Just t}",
            "\n        ",
        )
        + "\n"
        + r#"insertSound : SoundKey -> Audio.Source -> AssetPackBuilder -> AssetPackBuilder
insertSound id sound apb =
    case id of
        "#
        + &filter_map_join(
            input,
            AssetType::Sound,
            |a| a.name.to_string() + "->{apb|" + &a.name.to_camel_case() + "=Just sound}",
            "\n        ",
        )
}

fn try_build(input: &Input) -> String {
    let num_textures = input
        .iter()
        .filter(|a| a.t == AssetType::Texture)
        .collect::<Vec<&Asset>>()
        .len();
    let num_sounds = input
        .iter()
        .filter(|a| a.t == AssetType::Sound)
        .collect::<Vec<&Asset>>()
        .len();
    let num_tot = num_textures + num_sounds;

    r#"tryBuildAssetPack : AssetPackBuilder -> Maybe AssetPack
tryBuildAssetPack tpb =

    case ([ "#
        .to_owned()
        + &filter_map_join(
            input,
            AssetType::Texture,
            |a| "tpb.".to_owned() + &a.name.to_camel_case(),
            ",",
        )
        + "\n    ],["
        + &filter_map_join(
            input,
            AssetType::Sound,
            |a| "tpb.".to_owned() + &a.name.to_camel_case(),
            ",",
        )
        + "\n]) of\n    ([\n"
        + &(0..num_textures)
            .map(|n: usize| "Just t".to_owned() + &n.to_string())
            .collect::<Vec<String>>()
            .join(",")
        + "], ["
        + &(num_textures..num_tot)
            .map(|n: usize| "Just t".to_owned() + &n.to_string())
            .collect::<Vec<String>>()
            .join(",")
        + "    ]) ->    Just <|\n    {"
        + &input
            .iter()
            .zip(0..input.len())
            .map(|(a, n): (&Asset, usize)| a.name.to_camel_case() + "=t" + &n.to_string())
            .collect::<Vec<String>>()
            .join(",")
        + "\n    }\n    _ -> Nothing"
}

fn initial_builder(input: &Input) -> String {
    r#"initialAssetPackBuilder : AssetPackBuilder
initialAssetPackBuilder = {
"#
    .to_owned()
        + &input
            .iter()
            .map(|a| a.name.to_camel_case() + " = Nothing")
            .collect::<Vec<String>>()
            .join(",")
        + "}"
}

fn imports() -> String {
    r#"module Gen.AssetPack exposing (..)
import Audio
import Canvas.Texture exposing (Texture)"#
        .to_string()
}

fn texture_type_container(input: &Input) -> String {
    "type alias AssetPackContainer textureType soundType = \n{\n".to_owned()
        + &input
            .iter()
            .map(|a| {
                a.name.to_camel_case()
                    + " : "
                    + (if a.t == AssetType::Texture {
                        "textureType"
                    } else {
                        "soundType"
                    })
            })
            .collect::<Vec<String>>()
            .join(",")
        + "}"
        + r#"
type alias AssetPack = AssetPackContainer Texture Audio.Source
type alias AssetPackBuilder = AssetPackContainer (Maybe Texture) (Maybe Audio.Source)"#
}

fn filter_map_join<F>(input: &Input, filter: AssetType, map: F, join: &str) -> String
where
    F: FnMut(&Asset) -> String,
{
    input
        .iter()
        .filter(|a| a.t == filter)
        .map(map)
        .collect::<Vec<String>>()
        .join(join)
}

fn key_type_declaration(input: &Input) -> String {
    "type TextureKey = ".to_owned()
        + &filter_map_join(input, AssetType::Texture, |a| a.name.clone(), "|")
        + "\ntype SoundKey = "
        + &filter_map_join(input, AssetType::Sound, |a| a.name.clone(), "|")
}

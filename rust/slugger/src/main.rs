#[macro_use] extern crate rocket;

use rocket::{futures};
use rocket::fairing::{AdHoc};

use rocket::serde::{Serialize, Deserialize, json::Json};
use rocket_db_pools::{sqlx, Database, Connection};

use futures::{/*stream::TryStreamExt, */future::TryFutureExt};

#[derive(Database)]
#[database("sqlx")]
struct Db(sqlx::PgPool);

#[derive(Debug, Serialize, Deserialize)]
struct ProblemResult {
    n: i32,
    res: i32,
    n_requests: i32,
}

type Result<T, E = rocket::response::Debug<sqlx::Error>> = std::result::Result<T, E>;

#[get("/")]
fn index() -> &'static str {
    "I am a slow slugger"
}

#[get("/solve/<n>")]
async fn solve(mut db: Connection<Db>, n: i32) -> Result<Json<ProblemResult>> {

    let res = n * n;

    let _ = sqlx::query!("insert into test (n, result, created_at) values ($1, $2, now())", n, res)
        .execute(&mut *db)
        .await?;

    let res = sqlx::query!("select count(*) as cnt from test")
        .fetch_one(&mut *db)
        .map_ok(|r| ProblemResult{ n, res, n_requests: r.cnt.unwrap() as i32 })
        .await?;

    Ok(Json(res))
}

fn stage() -> AdHoc {
    AdHoc::on_ignite("SQLx Stage", |rocket| async {
        rocket.attach(Db::init())
            .mount("/", routes![
                index,
                solve,
            ])
    })
}

#[launch]
fn rocket() -> _ {
    rocket::build().attach(stage())
}

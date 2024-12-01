#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use actix_files::Files;
use actix_web::dev::Service;
use actix_web::{App, HttpServer};

#[actix_web::main]
async fn main() {
    HttpServer::new(|| {
        App::new().service(Files::new("/", "www")).wrap_fn(|sr, t| {
            let fut = t.call(sr);
            async {
                let x = fut.await?;

                Ok(x)
            }
        })
    })
    .bind(("127.0.0.1", 17821))
    .expect("port binding")
    .run()
    .await
    .expect("fail");

    println!("Hello, world!");
}

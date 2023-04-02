use actix_files::Files;
use actix_web::{App, HttpRequest, HttpServer, Responder, Route, web};
use actix_web::dev::{fn_service, Service};
use actix_web::http::header::{HeaderName, HeaderValue};
use actix_web::http::Method;

#[actix_web::main]
async fn main() {
    HttpServer::new(|| {
        App::new().service(
            Files::new("/", "www")
        ).wrap_fn(|sr, t| {
            let fut = t.call(sr);
            async {
                let mut x = fut.await?;
                // actix_files do not insert this header.
                // see https://github.com/actix/actix-web/discussions/3009 for more info.
                /*
                x.headers_mut().insert(
                    HeaderName::from_static("x-content-type-options"),
                    HeaderValue::from_static("nosniff")
                );

                 */

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

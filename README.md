A macro tool for bind any metadata to enum variants.

## Usage

```rust
use enum_meta::{derive_meta, Meta};

#[derive(Default, Clone)]
struct HTTPStatus(u32);

#[derive_meta(HTTPStatus)]
enum UserFacingError {
    #[meta(HTTPStatus(404))]
    NotFound,
    #[meta(HTTPStatus(400))]
    Invalid,
    #[meta(HTTPStatus(401))]
    PermissionDenied,
    #[meta(*self.0.clone())]
    Custom(HTTPStatus),
}

#[derive_meta(HTTPStatus)]
enum ErrorCode {
    #[meta(HTTPStatus(200))]
    Ok,
    #[meta(HTTPStatus(500))]
    IOError,
    #[meta(HTTPStatus(500))]
    DBError,
    #[meta(self.0.meta())]
    UserFacingError(UserFacingError),
}
```
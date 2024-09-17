#pragma once

#include <stddef.h>
#include <stdint.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef size_t usize;
typedef intptr_t isize;

typedef float f32;
typedef double f64;

#define U64_MAX ((u64)0xFFFFFFFFFFFFFFFF)
#define U32_MAX ((u32)0xFFFFFFFF)
#define U64_MIN ((u64)0)
#define I64_MAX ((i64)0x7FFFFFFFFFFFFFFF)
#define I64_MIN ((i64)0x8FFFFFFFFFFFFFFF)

#define U32_MIN ((u32)0)
#define I32_MAX ((i32)0x7FFFFFFF)
#define I32_MIN ((i32)0x80000000)

#define F32_EPSILON ((f32)1.19209290e-7F)
#define F64_EPSILON ((f64)2.2204460492503131e-16)
#define PI_HALF     (1.57079632679489661923)
#define PI          (3.14159265358979323846)
#define TAU         (6.28318530717958647692)
#define E           (2.71828182845904523536)
#define SQRT2       (1.41421356237309504880)

#define IS_ALIGN(x, align) (((x) & ((align)-1)) == 0)
#define IN_RANGE(x, min, max) ((x) >= (min) && x < (max))
#define ARRAY_COUNT(A) (sizeof(A)/sizeof(A[0]))

#define _GLUE2(A, B) A ## B
#define _GLUE(A, B) _GLUE2(A, B)
#define GLUE(A, B) _GLUE(A, B)
#define STATIC_ASSERT(c) typedef char GLUE(__SA__,__LINE__) [(c)?1:-1]

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define LIMIT_BOTTOM(x, minimum) MAX(x, minimum)
#define LIMIT_TOP(x, maximum) MIN(x, maximum)

#define MIN3(a, b, c) MIN(MIN(a, b), c)
#define MAX3(a, b, c) MAX(MAX(a, b), c)
#define CLAMP(x, lower, upper) MIN(MAX(x, lower), upper)
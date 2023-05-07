import exercises.generic.GenericFunctionExercises._


secret

secret
    .map(bytes => new String(bytes.toArray))
    .map(_.reverse)
    .swap
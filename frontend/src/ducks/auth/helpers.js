export const normalize = ({
  localizedLastName,
  localizedFirstName,
  lastName,
  firstName,
  profilePicture,
  ...rest
}) => ({
  ...rest,
  firstName: localizedFirstName,
  lastName: localizedLastName,
  lastNameWithLocale: lastName,
  firstNameWithLocale: firstName,
  profilePicture: {
    ...profilePicture,
    avatar:
      profilePicture["displayImage~"].elements[1].identifiers[0].identifier
  }
});

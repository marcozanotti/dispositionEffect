import setuptools

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setuptools.setup(
    name="dispositionEffect",
    version="0.0.1",
    author="Marco Zanotti, Jacopo Repossi",
    author_email="jacopo.repossi@gmail.com",
    description="dispositionEffect package",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/marcozanotti/dispositionEffect",
    project_urls={},
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    packages=setuptools.find_packages(where='src'),
    package_dir={'': 'src'},
    python_requires=">=3.4",
)